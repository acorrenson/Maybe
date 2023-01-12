type distrib =
  | Bernouilli of float
  | Uniform of int list

type expr =
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Add of expr * expr
  | Sample of distrib
  | Var of string
  | Cst of int

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
    Printf.fprintf f "U[%a]" print_list l
  | Var x -> Printf.fprintf f "%s" x
  | Cst c -> Printf.fprintf f "%d" c

and print_list f l =
  match l with
  | [] -> ()
  | [x] -> Printf.fprintf f "%d" x
  | x::xs -> Printf.fprintf f "%d %a" x print_list xs

let print_support f =
  List.iter (fun (x, p) ->
    Printf.fprintf f "%a ~ %f\n" print_expr x p
  )


let () = Random.self_init ()

let sample (d : distrib) : int =
  match d with
  | Bernouilli p ->
    if Random.float 1. >= p then 1 else 0
  | Uniform l ->
    List.nth l (Random.int (List.length l))

let rebind (env : string -> 'a) (x : string) (vx : 'a) =
  fun y -> if x = y then vx else env y

let rec exec (env : string -> int) (e : expr) : int =
  match e with
  | Let (x, e1, e2) ->
    let vx = exec env e1 in
    exec (rebind env x vx) e2
  | Add (e1, e2) ->
    exec env e1 + exec env e2
  | Sample d -> sample d
  | If (e1, e2, e3) ->
    begin match exec env e1 with
    | 0 -> exec env e2
    | 1 -> exec env e3
    | _ -> failwith "conditions should evaluates to 0 or 1"
    end
  | Var x -> env x
  | Cst c -> c

type support = (expr * float) list

let normalize (s : support) : support =
  let rec simpl e =
    match e with
    | Add (e1, e2) -> mk_add (simpl e1) (simpl e2)
    | Let (x, e1, e2) -> Let (x, simpl e1, simpl e2)
    | If (e1, e2, e3) -> If (simpl e1, simpl e2, simpl e3)
    | _ -> e
  and mk_add x y =
    match x, y with
    | Cst c, Cst c' -> Cst (c + c')
    | _ -> Add (x, y)
  in
  let rec group s =
    match s with
    | (x, p)::(y, q)::s when x = y ->
      group ((x, p +. q)::s)
    | (x, p)::s -> (x, p)::group s
    | [] -> []
  in
  s
  |> List.map (fun (x, p) -> simpl x, p)
  |> List.sort (fun (x, _) (y, _) -> compare x y)
  |> group

let sym_sample (d : distrib) : support =
  match d with
  | Bernouilli p ->
    [(Cst 1, p); (Cst 0, 1. -. p)]
  | Uniform l ->
    let p = 1. /. (float_of_int (List.length l)) in
    List.map (fun x -> (Cst x, p)) l

let sym_add (s1 : support) (s2 : support) : support =
  let ss1 = List.to_seq s1 in
  let ss2 = List.to_seq s2 in
  Seq.product ss1 ss2
  |> Seq.map (fun ((v1, p1), (v2, p2)) -> (Add (v1, v2), (p1 *. p2)))
  |> List.of_seq

module S = Set.Make(struct
  type t = expr
  let compare = compare
end)

let domain (l : support) =
  List.map fst l |> S.of_list

(** Takes 2 supports with different domains
    and return two equivalent supports over
    the same (ordered) domain
*)
let join s1 s2 : (support * support) =
  let dom1 = domain s1 in
  let dom2 = domain s2 in
  let ext1 = S.diff dom2 dom1 |> S.to_seq |> List.of_seq |> List.map (fun x -> x, 0.) in
  let ext2 = S.diff dom1 dom2 |> S.to_seq |> List.of_seq |> List.map (fun x -> x, 0.) in
  List.sort compare (s1 @ ext1), List.sort compare (s2 @ ext2)

let rec infer_aux (env : string -> support) (e : expr) : support =
  match e with
  | Let (x, e1, e2) ->
    let vx = infer_aux env e1 in
    infer_aux (rebind env x vx) e2
  | Add (e1, e2) ->
    sym_add (infer_aux env e1) (infer_aux env e2)
  | Sample d -> sym_sample d
  | If (e1, e2, e3) ->
    begin match infer_aux env e1 with
    | [(Cst 1, p); (Cst 0, np)] ->
      let supp1, supp2 = join (infer_aux env e2) (infer_aux env e3) in
      List.map2 (fun (v2, p2) (v3, p3) ->
        assert (v2 = v3);
        (v2, p *. p2 +. np *. p3)
      ) supp1 supp2
    | _ -> failwith "conditions should evaluates to 0 or 1"
    end
  | Var x ->
    Printf.printf "computing support for %s [%d]\n" x (List.length (env x));
    env x
  | Cst c -> [Cst c, 1.]
and infer (e : expr) : support =
  normalize (infer_aux (fun x -> [Var x, 1.]) e)



let my_prog =
  Let ("x", Sample (Bernouilli 0.5),
    Let ("y", Sample (Bernouilli 0.5),
      Let ("z", Sample (Bernouilli 0.5),
        (Add (Var "x", Add (Var "y", Var "z"))))))