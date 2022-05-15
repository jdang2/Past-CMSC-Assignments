open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
  | Int x -> Int_Val x
  | Bool x -> Bool_Val x
  | ID x -> if (List.mem_assoc x env) = false then raise (DeclareError("No binding found")) else (List.assoc x env)
  | Add (x, y) -> let one = (eval_expr env x) in 
                  let two = (eval_expr env y) in
                  (match one with
                  | Int_Val a -> (match two with
                                  | Int_Val b -> (Int_Val (a + b))
                                  | Bool_Val c -> raise(TypeError("Not an int")))
                  | Bool_Val a -> raise(TypeError("Not an int")))
  | Sub (x, y) -> let one = (eval_expr env x) in 
                  let two = (eval_expr env y) in
                  (match one with
                  | Int_Val a -> (match two with
                                  | Int_Val b -> (Int_Val (a - b))
                                  | Bool_Val c -> raise(TypeError("Not an int")))
                  | Bool_Val a -> raise(TypeError("Not an int")))
  | Mult (x, y) -> let one = (eval_expr env x) in 
                   let two = (eval_expr env y) in
                   (match one with
                   | Int_Val a -> (match two with
                                   | Int_Val b -> (Int_Val (a * b))
                                   | Bool_Val c -> raise(TypeError("Not an int")))
                   | Bool_Val a -> raise(TypeError("Not an int")))     
  | Div (x, y) -> let one = (eval_expr env x) in
                  let two = (eval_expr env y) in
                  (match one with
                    | Int_Val a -> (match two with
                                    | Int_Val b -> if b = 0 then raise(DivByZeroError) else (Int_Val (a / b))
                                    | Bool_Val c -> raise(TypeError("Not an int")))
                    | Bool_Val a -> raise(TypeError("Not an int"))) 
  | Pow (x, y) -> let one = (eval_expr env x) in
                  let two = (eval_expr env y) in
                  (match one with
                    | Int_Val a -> (match two with
                                    | Int_Val b -> 
                                        let temp = (float_of_int a) ** (float_of_int b) in
                                        (Int_Val (int_of_float temp))
                                    | Bool_Val c -> raise(TypeError("Not an int")))
                    | Bool_Val a -> raise(TypeError("Not an int"))) 
  
  | Or (x, y) -> let one = (eval_expr env x) in 
                 let two = (eval_expr env y) in
                 (match one with
                  | Bool_Val a -> (match two with
                                  | Bool_Val b -> (Bool_Val (a || b))
                                  | Int_Val c -> raise(TypeError("Not a bool")))
                  | Int_Val a -> raise(TypeError("Not a bool")))
  | And (x, y) -> let one = (eval_expr env x) in 
                  let two = (eval_expr env y) in
                  (match one with
                   | Bool_Val a -> (match two with
                                   | Bool_Val b -> (Bool_Val (a && b))
                                   | Int_Val c -> raise(TypeError("Not a bool")))
                   | Int_Val a -> raise(TypeError("Not a bool"))) 
  | Not (bool) -> let one = (eval_expr env bool) in
                  (match one with
                  | Bool_Val x -> (Bool_Val (not x))
                  | Int_Val x -> raise(TypeError("not a bool")))
                  
  | Greater(x, y) -> let one = (eval_expr env x) in
                      let two = (eval_expr env y) in
                      (match one with
                        | Int_Val a -> (match two with
                                        | Int_Val b -> (Bool_Val (a > b))
                                        | Bool_Val c -> raise(TypeError("Not an int")))
                        | Bool_Val a -> raise(TypeError("Not an int")))   

  | Less (x, y) -> let one = (eval_expr env x) in 
                   let two = (eval_expr env y) in
                   (match one with
                    | Int_Val a -> (match two with
                                    | Int_Val b -> (Bool_Val (a < b))
                                    | Bool_Val c -> raise(TypeError("Not an int")))
                    | Bool_Val a -> raise(TypeError("Not an int")))   
  | GreaterEqual (x, y) -> let one = (eval_expr env x) in
                           let two = (eval_expr env y) in
                           (match one with
                            | Int_Val a -> (match two with
                                            | Int_Val b -> (Bool_Val (a >= b))
                                            | Bool_Val c -> raise(TypeError("Not an int")))
                            | Bool_Val a -> raise(TypeError("Not an int")))   
  | LessEqual (x, y) -> let one = (eval_expr env x) in 
                        let two = (eval_expr env y) in
                        (match one with
                          | Int_Val a -> (match two with
                                          | Int_Val b -> (Bool_Val (a <= b))
                                          | Bool_Val c -> raise(TypeError("Not an int")))
                          | Bool_Val a -> raise(TypeError("Not an int")))   
  | Equal (x, y) -> let one = (eval_expr env x) in 
                    let two = (eval_expr env y) in 
                    (match one with
                      | Int_Val a -> (match two with
                                      | Int_Val b -> (Bool_Val (a = b))
                                      | Bool_Val c -> raise(TypeError("Not an int")))
                      | Bool_Val a -> (match two with
                                      | Int_Val b -> raise(TypeError("Not a bool"))
                                      | Bool_Val c -> (Bool_Val (a = c))))   
  | NotEqual (x, y) -> let one = (eval_expr env x) in 
                       let two = (eval_expr env y) in 
                       (match one with
                        | Int_Val a -> (match two with
                                        | Int_Val b -> (Bool_Val (a != b))
                                        | Bool_Val c -> raise(TypeError("Not an int")))
                        | Bool_Val a -> (match two with
                                        | Int_Val b -> raise(TypeError("Not a bool"))
                                        | Bool_Val c -> (Bool_Val (a != c))))   
                       
;;

let rec eval_stmt env s =
  match s with 
  | NoOp -> env
  | Seq (stmt1, stmt2) -> let environ1 = eval_stmt env stmt1 in 
                          let environ2 = eval_stmt environ1 stmt2 in 
                          environ2

  | Declare (x, var)-> if List.mem_assoc var env = true then raise(DeclareError("Variable already exists")) 
                       else (match x with 
                       | Int_Type -> (var, Int_Val(0)) :: env
                       | Bool_Type -> (var, Bool_Val(false)) :: env)

  | Assign (var, expr) -> if List.mem_assoc var env = false then raise(DeclareError("Variable doesn't exist"))
                          else let id = List.assoc var env in 
                               let evalex = eval_expr env expr in 
                               (match id with 
                                | Int_Val a -> (match evalex with
                                                | Int_Val b -> (var, evalex) :: env
                                                | Bool_Val b -> raise(TypeError("Wrong type")))
                                | Bool_Val a -> (match evalex with 
                                                | Bool_Val b -> (var, evalex) :: env
                                                | Int_Val b -> raise(TypeError("Wrong Type"))))
 | If (guard, ibody, ebody) -> let eval = eval_expr env guard in 
                               (match eval with 
                                | Bool_Val x -> let evaltwo = (if x = true then eval_stmt env ibody else eval_stmt env ebody) in
                                                evaltwo
                                | Int_Val x -> raise(TypeError("Can't do int")))
 | While (guard, body) -> let eval = eval_expr env guard in 
                          (match eval with 
                            | Bool_Val x -> (match x with 
                                              | true -> (eval_stmt (eval_stmt env body) (While(guard,body)))
                                              | false -> env)
                            | Int_Val x -> raise(TypeError("Invalid")))
 | For (var, startexpr, endexpr, body) -> let start = eval_expr env startexpr in
                                          let ed = eval_expr env endexpr in 
                                      
                                         (match start with 
                                          | Int_Val a -> let new_env = eval_stmt env (Assign(var, Int a)) in
                                                        (match ed with 
                                                        | Int_Val b -> if a <= b then (eval_stmt (eval_stmt new_env body) (For(var, Int (a+2), endexpr, body))) else new_env
                                                        | Bool_Val b-> raise(TypeError("Not an int")))
                                          | Bool_Val a-> raise(TypeError("not an int")))

 | Print (expr) -> let eval = eval_expr env expr in 
                   (match eval with 
                    | Int_Val x -> print_output_int x; print_output_newline(); env
                    | Bool_Val x -> print_output_bool x; print_output_newline(); env)
;;

 
      
