open Opal
open Lang

let (let*) = (>>=)

let parens (p : (char, 'a) parser) = between (exactly '(') (exactly ')') p

let integer' = many1 digit => implode % int_of_string
let integer = integer' => (fun i -> Cst i)
let ident = (letter <~> many alpha_num) => implode
let floating = (
    let* lhs = many1 digit in
    let* _   = token "." in
    let* rhs = many1 digit in
    return (lhs @ ['.'] @ rhs)
  ) => (fun l -> float_of_string (implode l))

let add = token "+" >> spaces >> return (fun x y -> Add (x, y))
let sub = token "-" >> spaces >> return (fun _ -> assert false)
let mul = token "*" >> spaces >> return (fun _ -> assert false)
let div = token "/" >> spaces >> return (fun _ -> assert false)

let rec parse_expr input = chainl1 parse_term (add <|> sub) input
and parse_term input = chainl1 parse_factor (mul <|> div) input
and parse_factor input = (parens parse_expr <|> parse_atom) input

and parse_atom input =
  (integer <|> parse_if <|> parse_sample <|> parse_let <|> parse_var) input

and parse_var input =
  (ident => (fun x -> Var x)) input

and parse_if input =
  begin
    let* _ = token "if" in
    let* c = spaces >> parse_expr in
    let* a = spaces >> between (token "{" << spaces) (token "}" << spaces) parse_expr in
    let* _ = token "else" in
    let* b = spaces >> between (token "{" << spaces) (token "}" << spaces) parse_expr in
    return (If (c, a, b))
  end input

and parse_let input =
  begin
    let* _ = token "let" in
    let* x = spaces >> ident in
    let* _ = token "=" in
    let* e1 = spaces >> parse_expr in
    let* _ = token "in" in
    let* e2 = spaces >> parse_expr in
    return (Let (x, e1, e2))
  end input

and parse_sample input =
  begin
    begin
      let* _ = token "B" in
      let* p = parens floating in
      return (Sample (Bernouilli p))
    end <|>
    begin
      let* _ = token "U[" in
      let* l = sep_by1 (spaces >> integer') (token ",") in
      let* _ = token "]" in
      return (Sample (Uniform l))
    end <|>
    begin
      let* _ = token "EU[" in
      let* l = sep_by1 (spaces >> parse_expr) (token ",") in
      let* _ = token "]" in
      return (Sample (EUniform l))
    end
  end input

let parse_file f =
  let inx = open_in f in
  let stream = LazyStream.of_channel inx in
  match (parse_expr << spaces) stream with
  | Some (x, Nil) -> x
  | _ -> failwith "parse error"