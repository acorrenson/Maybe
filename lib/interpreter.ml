open Lang

let () = Random.self_init ()

let rec sample (env : int store) (d : distribution) : int =
  match d with
  | Bernouilli p ->
    if Random.float 1. >= p then 1 else 0
  | Uniform l ->
    List.nth l (Random.int (List.length l))
  | EUniform l ->
    List.nth l (Random.int (List.length l))
    |> exec env

and exec (env : int store) (e : expr) : int =
  match e with
  | Let (x, e1, e2) ->
    let vx = exec env e1 in
    exec (rebind env x vx) e2
  | Add (e1, e2) ->
    exec env e1 + exec env e2
  | Sample d -> sample env d
  | If (e1, e2, e3) ->
    begin match exec env e1 with
    | 0 -> exec env e2
    | 1 -> exec env e3
    | _ -> failwith "conditions should evaluates to 0 or 1"
    end
  | Var x -> env x
  | Cst c -> c