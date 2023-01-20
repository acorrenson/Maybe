open Lang
open Distribution

(** Probabilistic addition *)
let (++) (s1 : distribution) (s2 : distribution) : distribution =
  let ss1 = List.to_seq s1 in
  let ss2 = List.to_seq s2 in
  Seq.product ss1 ss2
  |> Seq.map (fun ((v1, p1), (v2, p2)) -> (Add (v1, v2), (p1 *. p2)))
  |> List.of_seq

(** Sets of expressions *)
module S = Set.Make(struct
  type t = expr
  let compare = compare
end)

(** The domain of a distribution is its list of possible values *)
let domain (l : distribution) =
  List.map fst l |> S.of_list

(** Takes 2 supports with different domains
    and return two equivalent supports over
    the same (ordered) domain
*)
let join s1 s2 : (distribution * distribution) =
  let dom1 = domain s1 in
  let dom2 = domain s2 in
  let ext1 = S.diff dom2 dom1 |> S.to_seq |> List.of_seq |> List.map (fun x -> x, 0.) in
  let ext2 = S.diff dom1 dom2 |> S.to_seq |> List.of_seq |> List.map (fun x -> x, 0.) in
  List.sort compare (s1 @ ext1), List.sort compare (s2 @ ext2)

let extend_domain (dom : S.t) (d : distribution) : distribution =
  let ext =
    S.diff dom (domain d)
    |> S.to_seq
    |> List.of_seq
    |> List.map (fun x -> x, 0.)
  in
  List.sort compare (d @ ext)

let join_all (ld : distribution list) =
  let dom = List.(fold_left S.union S.empty (map domain ld)) in
  Printf.printf "dom: %a\n" print_expr_list (dom |> S.to_seq |> List.of_seq);
  List.map (extend_domain dom) ld

let rec mapn (f : 'a list -> 'b) (ll : 'a list list) : 'b list =
  if List.exists ((=) []) ll then []
  else
    let heads = List.map List.hd ll in
    let tails = List.map List.tl ll in
    (f heads)::mapn f tails

(** Compute the distribution of all possible values of a program *)
let rec infer (e : expr) : distribution =
  normalize (infer_aux (fun x -> [Var x, 1.]) e)

and infer_aux (env : distribution store) (e : expr) : distribution =
  match e with
  | Let (x, e1, e2) ->
    let vx = infer_aux env e1 in
    infer_aux (rebind env x vx) e2
  | Add (e1, e2) ->
    infer_aux env e1 ++ infer_aux env e2
  | Sample d -> sample env d
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
  | Var x -> env x
  | Cst c -> [Cst c, 1.]

and sample (env : distribution store) (d : Lang.distribution) : distribution =
  match d with
  | Bernouilli p ->
    [(Cst 1, p); (Cst 0, 1. -. p)]
  | Uniform l ->
    let p = 1. /. (float_of_int (List.length l)) in
    List.map (fun x -> (Cst x, p)) l
  | EUniform l ->
    let ld = join_all (List.map (infer_aux env) l) in
    let p = 1. /. (float_of_int (List.length ld)) in
    mapn (fun l ->
      Printf.printf "\n%a\n" print_distribution l;
      let v = List.hd l |> fst in
      assert (List.for_all ((=) v) (List.map fst l));
      (v, p *. List.fold_left (+.) 0. (List.map snd l))
    ) ld


