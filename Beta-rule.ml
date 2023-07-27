type 'a lambda = Var of 'a | Fun of 'a * 'a lambda | App of 'a lambda * 'a lambda

let rec beta_rule term =
  match term with
  | Var x -> Var x
  | Fun (x, t) -> Fun (x, beta_rule t)
  | App (Fun (x, t), r) -> substitute x r t
  | App (t1, t2) -> App (beta_rule t1, beta_rule t2)
  


and substitute var repl term =
  let new_var = var ^ "'" in
  let renamed_term = rename_variable var new_var term in
  substitute_no_capture new_var repl renamed_term

and rename_variable var new_var term =
  let rec rename term =
    match term with
    | Var x -> if x = var then Var new_var else Var x
    | Fun (x, t) ->
      if x = var then
        Fun (x, t) 
      else
        Fun (x, rename t)
    | App (t1, t2) -> App (rename t1, rename t2)
  in
  rename term

and substitute_no_capture var repl term =
  match term with
  | Var x -> if x = var then repl else Var x
  | Fun (x, t) ->
    if x = var then
      Fun (x, t)
    else if not (List.mem x (free_variables repl)) then
      Fun (x, substitute_no_capture var repl t)
    else
      let new_x = x ^ "'" in
      let renamed_t = rename_variable x new_x t in
      Fun (new_x, substitute_no_capture var repl renamed_t)
  | App (t1, t2) -> App (substitute_no_capture var repl t1, substitute_no_capture var repl t2)

and free_variables term =
  let rec collect_free_vars term vars =
    match term with
    | Var x -> x :: vars
    | Fun (x, t) -> collect_free_vars t (List.filter (fun y -> y <> x) vars)
    | App (t1, t2) -> collect_free_vars t1 vars |> collect_free_vars t2
  in
  collect_free_vars term []




  let rec get_lambda_string l = match l with
  | Fun (a, b) -> "λ" ^ a ^ "." ^ (get_lambda_string b)
  | App (a, b) -> (match a with
    | App _ | Var _ -> (get_lambda_string a) ^ (get_lambda_string b)
    | _ -> "(" ^ (get_lambda_string a) ^ ")" ^ (get_lambda_string b)
  )
  | Var a -> a;;
let test1 = App (App (Fun ("x", Fun ("y", Var "x")), Var "y"), Var "z");;
get_lambda_string test1 = "(λx.λy.x)yz";;
get_lambda_string (beta_rule test1) = "(λy'.y)z";;
get_lambda_string (beta_rule (beta_rule test1)) = "y";;
let test2 = App (Fun ("x", App (Var "x", Var "x")), Var "y");;
get_lambda_string test2 = "(λx.xx)y";;
get_lambda_string (beta_rule test2) = "yy";;
let test3 = App (Fun ("x", App (Fun ("y", App (Var "x", Var "y")), Var "z")), Var "l");;
get_lambda_string test3 = "(λx.(λy.xy)z)l";;
get_lambda_string (beta_rule test3) = "(λy.ly)z" || get_lambda_string (beta_rule test3) = "(λx.xz)l";;
get_lambda_string (beta_rule (beta_rule test3)) = "lz";;

(* let test4 = Fun ("x'", App (Fun ("y", Fun ("x", App (Var "x", Var "y"))), Var "x"));;
let test5 = App (Fun ("x'", Fun ("x'", Var "x'")), Var "a") *)