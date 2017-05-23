open Compile
open Runner
open Printf
open OUnit2
open MyTests

let forty_one = "sub1(42)"
let forty = "sub1(sub1(42))"
let def_x = "let x = 5 in x"
let def_x2 = "let x = 5 in sub1(x)"
let def_x3 = "let x = 5 in let x = 67 in sub1(x)"
let addnums = "5 + 10"
let single_nest = "(5 + 2) - 1"
let negatives = "(0 - 5) + (0 - 10)"
let negatives2 = "(0 - 1) + (0 - 1)"
let nested_add = "(5 + (10 + 20))"
let let_nested = "let x = (5 + (10 + 20)) in x * x"

let if_simple_true = "if true: 1 else: 2"
let if_simple_false = "if false: 1 else: 2"
let nested_if = "if if false: 1 else: true: 11 else: 2"

let greater_of_equal = "4 > 4"
let less_of_equal = "4 < 4"
let greater = "4 > 3"
let less = "3 < 4"
let not_greater = "2 > 3"
let not_less = "3 < 2"

let equal = "(0 - 2) == (0 - 2)"
let not_equal = "(0 - 1) == (0 - 2)"

let add_true_left   = "true + 4"
let add_true_right  = "1 + true"
let add_false_left  = "false + 4"
let add_false_right = "1 + false"

let overflow = "1073741823 + 2"
let underflow = "(0 - 1073741823) - 2"

let print = "print(add1(5))"

let err_if_simple_true = "if 0: 1 else: 2"
let err_if_simple_false = "if 54: 1 else: 2"
let err_nested_if = "if if 54: 1 else: 0: 11 else: 2"

let lottalet = "
let
  a = 1,
  b = print(a + 1),
  c = print(b + 1),
  d = print(c + 1),
  e = print(d + 1),
  f = print(e + 1),
  g = print(f + 1) in
g"

let lottalet2 ="
let
  a = 1,
  b = print(a + 1),
  c = print(b + 1),
  d = print(a + 1),
  e = print(b + 1),
  f = print(c + 1),
  g = print(a + 1) in
g"

let ibt = "isbool(true)"
let ibf = "isbool(false)"
let intr = "isnum(true)"
let infalse = "isnum(false)"
let ibz = "isbool(0)"
let ib1 = "isbool(1)"
let ibn1 = "isbool(-1)"
let inz = "isnum(0)"
let in1 = "isnum(1)"
let inn1 = "isnum(-1)"

let call1 = "
def f(x, y):
  x - y
f(1, 2)
"

let calls = [
  t "call1" call1 "-1"
]

let reg =
 [t "def_x" def_x "5";
  t "def_x2" def_x2 "4";
  t "def_x3" def_x3 "66";
  t "addnums" addnums "15";
  t "single_nest" single_nest "6";
  t "negatives" negatives "-15";
  t "negatives2" negatives2 "-2";
  t "nested_add" nested_add "35";
  t "let_nested" let_nested "1225";
  t "if_simple_true" if_simple_true "1";
  t "if_simple_false" if_simple_false "2";
  t "nested_if" nested_if "11";

  t "lottalet" lottalet "2\n3\n4\n5\n6\n7\n7";
  t "lottalet2" lottalet2 "2\n3\n2\n3\n4\n2\n2";

  t "greater_of_equal" greater_of_equal "false";
  t "less_of_equal" less_of_equal "false";
  t "greater" greater "true";
  t "less" less "true";
  t "not_greater" not_greater "false";
  t "not_less" not_less "false";

  t "equal" equal "true";
  t "not_equal" not_equal "false";

  t "print" print "6\n6";

  t "ibt" ibt "true";
  t "ibf" ibf "true";
  t "intrue" intr "false";
  t "infalse" infalse "false";
  t "ibz" ibz "false";
  t "ib1" ib1 "false";
  t "ibn1" ibn1 "false";
  t "inz" inz "true";
  t "in1" in1 "true";
  t "inn1" inn1 "true";

  terr "add_true_left"   add_true_left   "expected a number";
  terr "add_true_right"  add_true_right  "expected a number";
  terr "add_false_left"  add_false_left  "expected a number";
  terr "add_false_right" add_false_right "expected a number";

  terr "err_if_simple_true" err_if_simple_true "expected a boolean";
  terr "err_if_simple_false" err_if_simple_false "expected a boolean";
  terr "err_nested_if" err_nested_if "expected a boolean";

  terr "overflow" overflow "overflow";
  terr "underflow" underflow "overflow";
  ]
;;

let errs = [
  terr "prim1_1" "add1(true)" "expected a number" ;
  terr "prim1_2" "add1((1,2))" "expected a number" ;
  terr "prim1_3" "sub1(true)" "expected a number" ;
  terr "prim1_4" "sub1((1,2))" "expected a number" ;
  terr "prim1_5" "input(true)" "expected a number" ;
  terr "prim1_6" "input((1,2))" "expected a number" ;

  terr "unbound" "x" "Unbounded variable identifier x";
  terr "unbound2" "let x = 10 in y" "Unbounded variable identifier y";
  terr "multiple1" "let x = 1, x = 2 in x + 1" "Multiple bindings for variable identifier x";

  terr "duplicate" "def f(x, x): x\n9" "Duplicate parameter";
  terr "duplicate2" "def g(x): x\ndef f(x, x): x\n9" "Duplicate parameter";

  terr "duplicate_let" "let x = 10, x = 5 in x" "Multiple bindings for variable identifier x";
  terr "duplicate_let2" "let x = 10, y = 7, x = 5 in x" "Multiple bindings for variable identifier x";
  terr "duplicate_let3" "let x = 10, y = 7, y = 5 in x" "Multiple bindings for variable identifier y";
  terr "duplicate_let4" "def f(x): x + 5 let x = f(1,2) in x + 5" "Arity mismatch: f";

  terr "duplicate_fun" "def f(): 5\n def f(): 10\nf()" "Duplicate function";
  terr "duplicate_fun2" "def f(): 5\n def g(): 5\n def f(): 10\nf()" "Duplicate function";
]

(* ------------------------------------------------------------ *)

let input_tests = [
  t_i "input1" "input(0)" "42" ["42"];
  t_i "input2" "input(0) + input(1)" "85" ["42"; "43"];

  terr_i "inputerr1" "input(0) + 1" "expected a number" ["true"];
  terr_i "inputerr2" "if input(0): 1 else: 0" "expected a boolean" ["1"];
]

let istests = [
  t "istup_p1" "ispair((1,2))"          "true";
  t "istup_p2" "ispair(fst((1,2)))"     "false";
  t "istup_p3" "ispair(snd((1,(1,2))))" "true";
  t "istup_p4" "ispair(42)"             "false";
  t "istup_p5" "ispair(true)"           "false";
  t "istup_p6" "ispair(false)"          "false";

  t "istup_n1" "isnum((1,2))"          "false";
  t "istup_n2" "isnum(fst((1,2)))"     "true";
  t "istup_n3" "isnum(snd((1,(1,2))))" "false";
  t "istup_n4" "isnum(42)"             "true";
  t "istup_n5" "isnum(true)"           "false";
  t "istup_n6" "isnum(false)"          "false";

  t "istup_b1" "isbool((1,2))"          "false";
  t "istup_b2" "isbool(fst((1,2)))"     "false";
  t "istup_b3" "isbool(snd((1,(1,2))))" "false";
  t "istup_b4" "isbool(42)"             "false";
  t "istup_b5" "isbool(true)"           "true";
  t "istup_b6" "isbool(false)"          "true";
]

let equal_tests = [
  t "teq1" "let t = (1,2) in t == t"     "true";
  t "teq2" "(1,2) == (1,2)"              "false";
  t "teq3" "(1 == 2) == (true == false)" "true";
]

let tuple_p1 = "(snd((1,2)),snd((4,5)))"

let tuple_p2 = "
let t = (snd((8,9)),fst((10,13)))
in fst(t) + snd(t)"

let tuple_p3 = "
def len(t):
  if t == false: 0
  else: len(snd(t)) + 1

let t = (1,(2,(3,(4,false))))
in len(t)"

let tuple_p4 = "
def sum(l):
  if l == false:
    0
  else:
    fst(l) + sum(snd(l))

let t = (1,(2,(3,(4,false))))
in sum(t)"

let tuple_p5 = "
def sum(l):
  if l == false:
    0
  else:
    fst(l) + sum(snd(l))

def mul2(t):
  if t == false: false
  else: (fst(t) * 2,mul2(snd(t)))

let t = (1,(2,(3,(4,false)))),
    t2 = mul2(t)
in sum(t) * 2 == sum(t2)"

let tuple_p6 = "
def sum(l):
  if l == false:
    0
  else:
    fst(l) + sum(snd(l))

def append(l1,l2):
  if l1 == false:
    l2
  else:
    (fst(l1),append(snd(l1),l2))

let t1 = (1,(2,false)),
    t2 = (3,(4,false))
in  sum(append(t1,t2))"

let tuple_p10 = "
def dot(l1,l2):
  if l1 == false:
    0
  else:
    if l2 == false:
      0
    else:
      (fst(l1) * fst(l2)) + dot(snd(l1), snd(l2))

let t1 = (1,(2,(3,false))),
    t2 = (1,(2,(3,false)))
in
  dot(t1,t2)
"

let tuple_p11 = "fst(fst((((1,3),(2,5)))))"

let tuple_p12 = "snd(fst(((1,3),(2,5))))"

let tuple_p13 = "fst(snd(fst(((1,(3,4)),(2,5)))))"

let tuple_tests = [
  t "tup1"  "fst((4,5))" "4";
  t "tup2"  "fst(snd((4,(3,6))))" "3";
  t "tup4"  tuple_p1 "(2,5)";
  t "tup5"  tuple_p2 "19";
  t "tup6"  tuple_p3 "4";
  t "tup7"  tuple_p4 "10";
  t "tup8"  tuple_p5 "true";
  t "tup9"  tuple_p6 "10";
  t "tup10" tuple_p10 "14";
  t "tup11" tuple_p11 "1";
  t "tup12" tuple_p12 "3";
  t "tup13" tuple_p13 "3";
]

let pa4_tests = istests @ equal_tests @ input_tests @ tuple_tests

(* ------------------------------------------------------------ *)

let large_N = "1000000"

let pa5_tests = [
  t_i "tailcall1"           (src "tailcall1")           large_N [large_N];
  t   "tailcall_overwrite1" (src "tailcall_overwrite1") "10";
  t   "tailcall_overwrite2" (src "tailcall_overwrite2") "37";
  t   "tailcall_overwrite3" (src "tailcall_overwrite3") "4";
  t   "lessthan1"           "1 < 1"                     "false";
  t   "greaterthan1"        "1 > 1"                     "false";
  t   "lessthan2"           "100 < 3"                   "false";
  t   "greaterthan2"        "100 > 3"                   "true";
  t_i "even_odd"            (src "even_odd")            "true"  [large_N];
]

(* ------------------------------------------------------------ *)

let suite =
"suite">:::
   calls @ reg @ errs @
   pa4_tests @
   pa5_tests @
   myTestList


let () =
  run_test_tt_main suite
;;
