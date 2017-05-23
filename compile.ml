open Printf
open Expr
open Instruction
open Misc

type 'a envt = (string * 'a) list

let const_true  = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let type_mask ty = HexConst
    (match ty with
     | TNum   -> 0x1
     | TBool  -> 0x3
     | TPair  -> 0x3
    )

let type_tag ty = HexConst
    (match ty with
     | TNum   -> 0x0
     | TBool  -> 0x3
     | TPair  -> 0x1
    )

let typecheck ty =
  [ IAnd (Reg EAX, type_mask ty)
  ; ICmp (Reg EAX, type_tag ty)
  ]

let compare cmp =
  let jump_of_cmp l = match cmp with
    | Less -> IJl l | Equal -> IJe l | Greater -> IJg l
    | _ -> failwith "impossible"
  in
  let label_true = gen_temp "true" in
  let label_done = gen_temp "done" in
  [ jump_of_cmp label_true
  ; IMov (Reg EAX, const_false)
  ; IJmp label_done
  ; ILabel label_true
  ; IMov (Reg EAX, const_true)
  ; ILabel label_done
  ]

let is_type ty = typecheck ty @ compare Equal

let throw_err code =
  [ IPush (Sized(DWORD_PTR, Const code))
  ; ICall "error"
  ]

let error_overflow  = "overflow_check"
let error_non_int   = "error_non_int"
let error_non_bool  = "error_non_bool"
let error_non_pair  = "error_non_pair"
let error_too_small = "error_too_small"
let error_too_large = "error_too_large"

let check_overflow  = IJo(error_overflow)

let check_eax_num = typecheck TNum @ [ IJne(error_non_int) ]

let check_eax_pair = typecheck TPair @ [ IJne error_non_pair ]

let stackloc      i =   RegOffset (-4 * i, ESP)
let save_to_stack i = [ IMov (stackloc i, Reg(EAX)) ]
let restore_stack i = [ IMov (Reg(EAX), stackloc i) ]

let c_call si f =
  [ ISub (Reg ESP, Const (4 * si))
  ; IPush (Sized (DWORD_PTR, Reg EAX))
  ; ICall f
  ; IAdd (Reg ESP, Const (4 * (si +1)))
  ]

(* make a call to try_gc (if necessary)

   arguments: 

     - size: the size of the heap space we want to allocate (in bytes)
   
     - si: current stack index
*)
let reserve (size : int) (si : int) : instruction list =
  failwith "TODO: reserve @ compile.ml"

let rec compile_prim1 (o : prim1) (e : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  let prelude = compile_expr e false si env in
  let isnum   = save_to_stack si @ check_eax_num  @ restore_stack si in
  let ispair  = save_to_stack si @ check_eax_pair @ restore_stack si in
  let tuple_access tuple ind =
    let tuple_inst  = compile_expr tuple false si env @ save_to_stack si @ check_eax_pair in
    let elem_inst   = [ IMov(Reg EAX, stackloc si)
                      ; IMov(Reg EAX, RegOffset(ind*4 + 3, EAX))
                      ] in
    tuple_inst @ elem_inst
  in
  let op_is   = match o with
    | Add1    -> isnum @ [ IAdd(Reg(EAX), Const(2)) ]
    | Sub1    -> isnum @ [ IAdd(Reg(EAX), Const(-2)) ]
    | IsNum   -> is_type TNum
    | IsBool  -> is_type TBool
    | IsPair  -> is_type TPair
    | Print   -> c_call si "print"
    | Input   -> isnum @ c_call si "input"
    | Fst     -> ispair @ tuple_access e 0
    | Snd     -> ispair @ tuple_access e 1

  in prelude @ op_is

and compile_prim2 (o : prim2) (e1 : expr) (e2 : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  let tuple_set tuple exp ind =
    let tuple_inst  = compile_expr tuple false si env @ save_to_stack si @ check_eax_pair in
    let exp_inst    = compile_expr exp false (si+1) env @ save_to_stack (si+1) in
    let elem_inst   = [ IMov(Reg ECX, stackloc si)
                      ; IMov(Reg EAX, stackloc (si+1))
                      ; IMov(RegOffset(ind*4 + 3, ECX), Reg EAX)
                      ] in
    tuple_inst @ exp_inst @ elem_inst
  in
  match o with
  | Equal  -> compile_equals e1 e2 istail si env
  | SetFst -> tuple_set e1 e2 0
  | SetSnd -> tuple_set e1 e2 1
  | Plus | Minus | Times | Less | Greater
    -> compile_prim2_helper o e1 e2 istail si env

and compile_equals (e1 : expr) (e2 : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  compile_expr e2 false si env
  @ save_to_stack si
  @ compile_expr e1 false (si+1) env
  @ [ ICmp (Reg EAX, stackloc si) ]
  @ compare Equal

and compile_prim2_helper (o : prim2) (e1 : expr) (e2 : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si+1) in
  let e1_is   = compile_expr e1 false si env     @ save_to_stack si     @ check_eax_num in
  let e2_is   = compile_expr e2 false (si+1) env @ save_to_stack (si+1) @ check_eax_num in
  let op_is   = match o with
    | Plus    -> [ IAdd (Reg(EAX), rhs_loc); check_overflow ]
    | Minus   -> [ ISub (Reg(EAX), rhs_loc); check_overflow ]
    | Times   -> [ ISar (Reg(EAX), Const(1))
                 ; IMul (Reg(EAX), rhs_loc)
                 ; check_overflow
                 ]
    | Less
    | Greater -> ICmp (Reg(EAX), rhs_loc) :: compare o
    | Equal | SetFst | SetSnd -> []

  in     e1_is
         @ e2_is
         @ restore_stack si
         @ op_is

and compile_let (bs : (string * expr) list) (body : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  match bs with
  | []         -> compile_expr body istail si env
  | (x,e)::bs' ->
    (compile_expr e false si env)
    @ save_to_stack si
    @ compile_let bs' body istail (si+1) ((x,si)::env)

and compile_if (cond : expr) (e_then : expr) (e_else : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  let cond_is    = compile_expr cond   false si env in
  let then_is    = compile_expr e_then istail si env in
  let else_is    = compile_expr e_else istail si env in
  let label_then = gen_temp "then" in
  let label_else = gen_temp "else" in
  let label_end  = gen_temp "end"  in
  cond_is
  @ [ ICmp(Reg(EAX), const_true)
    ; IJe(label_then)
    ; ICmp(Reg(EAX), const_false)
    ; IJe(label_else)
    ; IJmp(error_non_bool)
    ; ILabel(label_then)
    ]
  @ then_is
  @ [ IJmp(label_end); ILabel(label_else) ]
  @ else_is
  @ [ ILabel(label_end) ]

and compile_app (f : string) (args : expr list)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  let put_args es offset env =
    let put1 i e = compile_expr e false (offset + i) env
                   @ [ IMov (Sized(DWORD_PTR,stackloc (offset + i)), Reg EAX) ]
    in flatmapi put1 es
  in
  let compile_app_normal () =
    let lbl = gen_temp "after_call" in
    [ IMov(Sized(DWORD_PTR, stackloc si), Label lbl)
    ; IMov(stackloc (si+1), Reg ESP) ]
    @ put_args args (si+2) env
    @ [ ISub(Reg ESP, Const (si*4))
      ; IJmp f
      ; ILabel lbl
      ; IMov(Reg ESP, stackloc 2) ]
  in
  let compile_app_tailcall () =
    put_args args si env
    @ (flatmapi (fun i _ -> [ IMov (Reg EAX, Sized(DWORD_PTR, stackloc (si+i)))
                            ; IMov (Sized(DWORD_PTR, stackloc (2+i)), Reg EAX)])
         args)
    @ [ IJmp f ]
  in
  if istail
  then compile_app_tailcall ()
  else compile_app_normal ()

and compile_pair (e1: expr) (e2: expr) (istail: bool) (si: int) (env: int envt) : instruction list =
  let args = [e1; e2] in
  (* 1. compile elements into the stack *)
  let (_,compile_elements) =
    let f (n,l) e =
      let si' = si + n in
      let l'  = l @ compile_expr e false si' env @ save_to_stack si' in
      let n'  = n + 1 in
      (n', l')
    in
    List.fold_left f (0,[]) args
  in
  let si' = si + 2 in
  (* 2. make sure that we can allocate 3 words for the pair *)
  let alloc_space = reserve 12 si' in
  (* 3. set the gc word to 0 *)
  let metadata =
    let dw n = Sized (DWORD_PTR, (Const n)) in
    [ IMov(RegOffset(0, EBX), dw 0)
    ] in
  (* 4. move elements from stack into the pair *)
  let (_, put_elements) =
    let f (n,l) _ =
      let l'  = l @ restore_stack (si+n) @ [ IMov(RegOffset((n+1)*4, EBX), Reg EAX) ] in
      let n'  = n + 1 in
      (n', l')
    in
    List.fold_left f (0,[]) args
  in
  (* 5. update EAX and increment EBX *)
  let put_result = [ IMov(Reg EAX, Reg EBX)
                   ; IOr(Reg EAX, Const 1)
                   ; IAdd(Reg EBX, Const 12)
                   ] in
  compile_elements @ alloc_space @ metadata @ put_elements @ put_result


and compile_expr (e : expr)
    (istail : bool) (si : int) (env : int envt) : instruction list =
  match e with
  | ENumber (n ) -> [ IMov (Reg(EAX), Const((n lsl 1))) ]
  | EBool   (b ) -> [ IMov (Reg(EAX), if b then const_true else const_false) ]
  | EId     (rx) ->
    let arg = begin match find env rx with
      | Some(i) -> stackloc i
      | None    -> failwith "Unbounded variable identifier"
    end
    in [ IMov (Reg(EAX), arg) ]
  | EPrim1 (op, e')          -> compile_prim1    op e'         istail si env
  | EPrim2 (op, e1, e2)      -> compile_prim2    op e1 e2      istail si env
  | ELet   (bs, body)        -> compile_let      bs body       istail si env
  | EIf    (c, ethen, eelse) -> compile_if       c ethen eelse istail si env
  | EApp   (f, args)         -> compile_app      f args        istail si env
  | EPair  (e1,e2)           -> compile_pair     e1 e2         istail si env
  | ESeq exprs ->
    let f e (tc,is) = (false, compile_expr e tc si env @ is) in
    let _, is = List.fold_right f exprs (istail, [])
    in is

let compile_decl (d : decl) : instruction list =
  let DFun (fname, args, body) = d in
  let env = List.rev (List.mapi (fun i s -> (s, i+2)) args) in
  let si = List.length args + 2 in
  [ ILabel fname ] @ compile_expr body true si env @ [ IRet ]

let find_decl (ds : decl list) (name : string) : decl option =
  find_f (fun (DFun(fname,_,_)) -> fname = name) ds

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  match e with
  | ENumber(n) -> 
    if   n > 1073741823 || n < -1073741824
    then ["Error: Integer overflow"]
    else []
  | EBool(_) -> []
  | EPair(e1, e2)   ->
    (well_formed_e e1 ds env) @ (well_formed_e e2 ds env)
  | EId(x) ->
    begin match find env x with
      | None -> ["Unbounded variable identifier " ^ x]
      | Some(_) -> []
    end
  | EPrim1(op, e) ->
    well_formed_e e ds env
  | EPrim2(op, left, right) ->
    (well_formed_e left ds env) @ (well_formed_e right ds env)
  | EIf(cond, thn, els) ->
    (well_formed_e cond ds env) @
    (well_formed_e thn ds env) @
    (well_formed_e els ds env)
  | EApp(name, args) ->
    let from_args = List.flatten (List.map (fun a -> well_formed_e a ds env) args) in
    begin match find_decl ds name with
      | None -> ("No such function: " ^ name)::from_args
      | Some(DFun(_,params,_)) ->
        if List.length params = List.length args
        then from_args
        else ("Arity mismatch: " ^ name):: from_args

    end
  | ELet(binds, body) ->
    let names          = List.map fst binds in
    let env_from_binds = List.map (fun a -> (a, true)) names in
    let from_body      = well_formed_e body ds (env_from_binds @ env) in
    let dup_binds =
      begin match find_dup names with
        | None       -> []
        | Some(name) -> ("Multiple bindings for variable identifier " ^ name)::from_body
      end
    in
    let (from_binds, _) =
      let f (errs, env) (name, be) =
        let errs' = well_formed_e be ds env in
        let env'  = (name,true)::env in
        (errs @ errs', env') in
      List.fold_left f ([], env) binds
    in
    dup_binds @ from_binds @ from_body
  | ESeq exprs ->
    let f is e = is @ well_formed_e e ds env in
    let base   = [] in
    List.fold_left f base exprs


let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
  | DFun(name, args, body) ->
    let env = List.map (fun a -> (a, true)) args in
    let from_body = well_formed_e body ds env in
    begin match find_dup args with
      | None -> from_body
      | Some(v) -> ("Duplicate parameter " ^ v)::from_body
    end

let well_formed_p (p : program) : string list =
  match p with
  | Program(ds, maine) ->
    let names = List.map (fun (DFun(name, _, _)) -> name) ds in
    let subexpr_errs = (well_formed_e maine ds []) @
                       (List.flatten (List.map (fun d -> well_formed_d d ds) ds)) in
    begin match find_dup names with
      | None -> subexpr_errs
      | Some(v) -> ("Duplicate function " ^ v)::subexpr_errs
    end

let compile_to_string prog =
  match well_formed_p prog with
  | x::rest ->
    let errstr = String.concat "\n" (x::rest) in
    failwith (errstr ^ "\n")
  | [] ->
    match prog with
    | Program(decls, main) ->
      let compiled_decls = List.map compile_decl decls in
      (* start with si=2 to maintain consistency (old ESP) *)
      let compiled_main = (compile_expr main true 2 []) in
      let prelude = "
section .text
extern error
extern print
extern equal
extern try_gc
extern input
extern HEAP_END
extern STACK_BOTTOM
global our_code_starts_here" in
      let main_start = [
        ILabel("our_code_starts_here");
        IMov(Reg(EBX), RegOffset(4, ESP));
        IMov(LabelContents("STACK_BOTTOM"), Reg(ESP)); (* highest stack address (return address to C) *)
        IMov(RegOffset(-4, ESP), Sized(DWORD_PTR, Const 0)); (* set old ESP to 0 *)
      ] in
      let postlude = [
        IRet
      ]
        @ [ILabel(error_non_int)]    @ (throw_err 1)
        @ [ILabel(error_non_bool)]   @ (throw_err 2)
        @ [ILabel("overflow_check")] @ (throw_err 3)
        @ [ILabel(error_non_pair)]   @ (throw_err 4)
      in
      let as_assembly_string = (to_asm (
          (List.flatten compiled_decls) @
          main_start @
          compiled_main @
          postlude)) in
      sprintf "%s%s\n" prelude as_assembly_string
