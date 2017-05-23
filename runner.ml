open Unix
open Filename
open Str
open Compile
open Printf
open OUnit2
open ExtLib
open Lexing

let timeout = 5.0 (* sec *)

let globCounter = ref 0

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let either_printer e =
  match e with
    | Left(v) -> sprintf "Error: %s\n" v
    | Right(v) -> v

let string_of_position p =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let parse name lexbuf : Expr.program =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
    |  Failure "lexing: empty token" ->
         failwith (sprintf "lexical error at %s"
                        (string_of_position lexbuf.lex_curr_p))

(* Read a file into a string *)
let string_of_file (file_name : string) : string =
  let inchan = open_in file_name in
  let str = input_all inchan in
  close_in inchan;
  str

let parse_string (name : string) (s : string) : Expr.program = 
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let parse_file (name : string) (input_file : in_channel) : Expr.program = 
  let s     = input_all input_file in
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let compile_file_to_string name input_file =
  let input_program = parse_file name input_file in
  (compile_to_string input_program);;

let compile_string_to_string name s =
  let input_program = parse_string name s in
  (compile_to_string input_program);;

let make_tmpfiles name =
  let (null_stdin, _) = pipe() in
  let stdout_name = (temp_file ("stdout_" ^ name) ".out") in
  let stdin_name = (temp_file ("stderr_" ^ name) ".err") in
  (openfile stdout_name [O_RDWR] 0o600, stdout_name,
   openfile stdin_name [O_RDWR] 0o600, stdin_name,
   null_stdin)

type result = (string, string) either

let run_no_vg (program_name : string) args : result =
  let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "run" in
  let ran_pid = Unix.create_process (program_name ^ ".run") (Array.of_list ([""] @ args)) rstdin rstdout rstderr in
  let _timeout = Thread.create (fun () ->
    Thread.delay timeout;
    Unix.kill ran_pid 9
  ) () in
  let (_, status) = waitpid [] ran_pid in
  let result = match status with
    | WEXITED 0 -> Right(string_of_file rstdout_name)
    | WEXITED n -> Left(sprintf "Error %d: %s" n (string_of_file rstderr_name))
    | WSIGNALED n ->
      Left(sprintf "Signalled with %d while running %s." n program_name)
    | WSTOPPED n ->
      Left(sprintf "Stopped with signal %d while running %s." n program_name) in
  List.iter close [rstdout; rstderr; rstdin];
  List.iter unlink [rstdout_name; rstderr_name];
  result


let run_vg (program_name : string) args : result =
  let (rstdout, rstdout_name, rstderr, rstderr_name, rstdin) = make_tmpfiles "run" in
  let ran_pid = Unix.create_process "valgrind"  (Array.of_list ([""; (program_name ^ ".run")] @ args)) rstdin rstdout rstderr in
  let (_, status) = waitpid [] ran_pid in
  let vg_str = string_of_file rstderr_name in
  let vg_ok = String.exists vg_str "0 errors from 0 contexts" in
  let result = match (status, vg_ok) with
    | WEXITED 0, true -> Right(string_of_file rstdout_name)
    | WEXITED 0, false -> Left("Stdout: " ^ (string_of_file rstdout_name) ^ "\n" ^ "Valgrind: \n" ^ vg_str)
    | WEXITED n, _ -> Left(sprintf "Error %d: %s" n vg_str)
    | WSIGNALED n, _ ->
      Left(sprintf "Signalled with %d while running %s." n program_name)
    | WSTOPPED n, _ ->
      Left(sprintf "Stopped with signal %d while running %s." n program_name) in
  List.iter close [rstdout; rstderr; rstdin];
  List.iter unlink [rstdout_name; rstderr_name];
  result

let run p (out : string) (runner : string -> (string list) -> result) args : result =
  let maybe_asm_string =
    try Right(compile_to_string p)
    with Failure s -> 
      Left("Compile error: " ^ s)
  in    
  match maybe_asm_string with
  | Left(s) -> Left(s)
  | Right(asm_string) ->
    let outfile = open_out (out ^ ".s") in
    fprintf outfile "%s" asm_string;
    close_out outfile;
    let (bstdout, bstdout_name, bstderr, bstderr_name, bstdin) = make_tmpfiles "build" in
    let built_pid = Unix.create_process "make" (Array.of_list [""; out ^ ".run"]) bstdin bstdout bstderr in
    let (_, status) = waitpid [] built_pid in

    let try_running = match status with
    | WEXITED 0 ->
      Right(string_of_file bstdout_name)
    | WEXITED n ->
      Left(sprintf "Finished with error while building %s:\n%s" out (string_of_file bstderr_name))
    | WSIGNALED n ->
      Left(sprintf "Signalled with %d while building %s." n out)
    | WSTOPPED n ->
      Left(sprintf "Stopped with signal %d while building %s." n out) in

    let result = match try_running with
    | Left(_) -> try_running
    | Right(msg) ->
      runner out args in

    List.iter close [bstdout; bstderr; bstdin];
    List.iter unlink [bstdout_name; bstderr_name];
    result

let cmp_results check result = 
  match check, result with
  | Right(expect_msg), Right(actual_msg)
  | Left(expect_msg), Left(actual_msg) ->
    let r = Str.regexp ", *" in
    let f s = Str.global_replace r "," s in
    let expect_msg' = f expect_msg in
    let actual_msg' = f actual_msg in
    String.exists actual_msg' expect_msg'
  | _ -> false

let test_run args program_str outfile expected test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_no_vg args in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer ~cmp:cmp_results

let test_run_valgrind args program_str outfile expected test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_vg args in
  assert_equal (Right(expected ^ "\n")) result ~printer:either_printer ~cmp:cmp_results

let test_err args program_str outfile errmsg test_ctxt =
  let full_outfile = "output/" ^ outfile in
  let program = parse_string outfile program_str in
  let result = run program full_outfile run_no_vg args in
  assert_equal (Left(errmsg)) result ~printer:either_printer ~cmp:cmp_results

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let src f = load_file ("input/" ^ f ^ ".garter")

let quot a = "\"" ^ a ^ "\"";;
let brac a = "["  ^ a ^ "]";;

type test_kind = Pos | Neg
                 
let test_kind_to_str = function | Pos -> "pos"
                                | Neg -> "neg"

let suite_print (k: test_kind) (name: string) (program: string) (expected: string) (input: string list) : unit =
  let k        = test_kind_to_str k |> quot in
  let n        = quot name in
  let helper s = String.nsplit s "\n" |> List.map quot |> String.join ", " |> brac in
  let ps       = helper program in
  let is       = List.map quot input |> String.join ", " |> brac in
  let es       = helper expected in
  let _output  = sprintf "{
  \"kind\":     %s,
  \"name\":     %s,
  \"program\":  %s,
  \"input\":    %s,
  \"expected\": %s,
}" k n ps is es in
  (* print_string _output; print_string ","; print_newline () *)
  ()

let default_heap_size = "100000"

let to_args (args : string list) : string list = default_heap_size :: args

let t   name program expected                     = 
  suite_print Pos name program expected [];
  name>::test_run [] program name expected;;
let tgc name heap_size program expected           = 
  let args' = [string_of_int heap_size] in
  suite_print Pos name program expected args';
  name>::test_run args' program name expected;;
let t_i   name program expected args              =
  let args' = to_args args in
  suite_print Pos name program expected args';
  name>::test_run args' program name expected ;;
let tgc_i name heap_size program expected args    =
  let args' = string_of_int heap_size :: args in
  suite_print Pos name program expected args';
  name>::test_run args' program name expected ;;

let terr   name program expected                  = 
  suite_print Neg name program expected [];
  name>::test_err [] program name expected;;
let tgcerr name heap_size program expected        = 
  let args' = [string_of_int heap_size] in
  suite_print Neg name program expected args';
  name>::test_err args' program name expected;;
let terr_i name program expected args             = 
  let args' = to_args args in
  suite_print Neg name program expected args';
  name>::test_err args' program name expected ;;
let tgcerr_i name heap_size program expected args = 
  let args' = string_of_int heap_size :: args in
  suite_print Neg name program expected args';
  name>::test_err args' program name expected;;
