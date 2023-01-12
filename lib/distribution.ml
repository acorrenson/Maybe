open Lang

(**
  A distribution is a list of expressions
  together with their associated probabilities.
  The probabilities should sum up to 1.
*)
type distribution = (expr * float) list

(** Normalize a distriution by grouping equivalent expressions *)
let normalize (d : distribution) : distribution =
  let rec group s =
    match s with
    | (x, p)::(y, q)::s when x = y ->
      group ((x, p +. q)::s)
    | (x, p)::s -> (x, p)::group s
    | [] -> []
  in
  d
  |> List.map (fun (x, p) -> simpl x, p)
  |> List.sort (fun (x, _) (y, _) -> compare x y)
  |> group

let print_distribution f =
  List.iter (fun (x, p) ->
    Printf.fprintf f "%a ~ %f\n" print_expr x p
  )