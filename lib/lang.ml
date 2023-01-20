(** Common distributions *)
type distribution =
  | Bernouilli of float
  | Uniform of int list
  | EUniform of expr list

(** Expressions *)
and expr =
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Add of expr * expr
  | Sample of distribution
  | Var of string
  | Cst of int

(** Stores are mappings from variables to a given domain *)
type 'a store = string -> 'a

(** Pretty printer for [expr] *)
let rec print_expr f = function
  | Let (x, e1, e2) ->
    Printf.fprintf f "let %s = %a in %a" x print_expr e1 print_expr e2
  | Add (e1, e2) ->
    Printf.fprintf f "(%a + %a)" print_expr e1 print_expr e2
  | If (e1, e2, e3) ->
    Printf.fprintf f "if (%a) { %a } else { %a }" print_expr e1 print_expr e2 print_expr e3
  | Sample (Bernouilli p) ->
    Printf.fprintf f "B(%f)" p
  | Sample (Uniform l) ->
    Printf.fprintf f "U[%a]" print_int_list l
  | Sample (EUniform l) ->
    Printf.fprintf f "U[%a]" print_expr_list l
  | Var x -> Printf.fprintf f "%s" x
  | Cst c -> Printf.fprintf f "%d" c

(** Pretty printer for lists of integers *)
and print_int_list f l =
  match l with
  | [] -> ()
  | [x] -> Printf.fprintf f "%d" x
  | x::xs -> Printf.fprintf f "%d %a" x print_int_list xs

and print_expr_list f l =
  match l with
  | [] -> ()
  | [x] -> Printf.fprintf f "%a" print_expr x
  | x::xs -> Printf.fprintf f "%a %a" print_expr x print_expr_list xs

(** Naive bottom-up expression simplifier *)
let rec simpl e =
  match e with
  | Add (e1, e2) -> mk_add (simpl e1) (simpl e2)
  | Let (x, e1, e2) -> Let (x, simpl e1, simpl e2)
  | If (e1, e2, e3) -> If (simpl e1, simpl e2, simpl e3)
  | _ -> e

(** Smart constructor for additions *)
and mk_add x y =
  match x, y with
  | Cst c, Cst c' -> Cst (c + c')
  | _ -> Add (x, y)

(** Rebind a key in a store *)
let rebind (env : string -> 'a) (x : string) (vx : 'a) =
  fun y -> if x = y then vx else env y
