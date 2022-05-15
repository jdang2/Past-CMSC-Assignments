open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_or toks

and parse_or tok = 
  let (tok_second, f1) = (parse_and tok) in
  match tok_second with 
  | Tok_Or :: t -> let tok_third = (match_token tok_second Tok_Or) in 
                   let (tok_for, f2) = (parse_or tok_third) in 
                   (tok_for, Or(f1, f2))
  | _ -> (tok_second, f1)

and parse_and tok =
  let (tok_second, f1) = (parse_equality tok) in
  match tok_second with 
  | Tok_And :: t -> let tok_third = (match_token tok_second Tok_And) in 
                    let (tok_for, f2) = (parse_and tok_third) in 
                    (tok_for, And(f1, f2))
  | _ -> (tok_second, f1)

and parse_equality tok =
  let (tok_second, f1) = (parse_relation tok) in
  match tok_second with
  | Tok_Equal :: t -> let tok_third = (match_token tok_second Tok_Equal) in 
                      let (tok_for, f2) = (parse_equality tok_third) in 
                      (tok_for, Equal(f1, f2))
  | Tok_NotEqual :: t -> let tok_third = (match_token tok_second Tok_NotEqual) in 
                         let (tok_for, f2) = (parse_equality tok_third) in 
                         (tok_for, NotEqual(f1, f2))
  | _ -> (tok_second, f1)

and parse_relation tok =
  let (tok_second, f1) = (parse_additive tok) in
  match tok_second with
  | Tok_Less :: t -> let tok_third = (match_token tok_second Tok_Less) in
                     let (tok_for, f2) = (parse_relation tok_third) in 
                     (tok_for, Less(f1, f2))
  | Tok_Greater :: t -> let tok_third = (match_token tok_second Tok_Greater) in
                     let (tok_for, f2) = (parse_relation tok_third) in 
                     (tok_for, Greater(f1, f2))
  | Tok_LessEqual :: t -> let tok_third = (match_token tok_second Tok_LessEqual) in
                     let (tok_for, f2) = (parse_relation tok_third) in 
                     (tok_for, LessEqual(f1, f2))
  | Tok_GreaterEqual :: t -> let tok_third = (match_token tok_second Tok_GreaterEqual) in
                     let (tok_for, f2) = (parse_relation tok_third) in 
                     (tok_for, GreaterEqual(f1, f2))
  | _ -> (tok_second, f1)

and parse_additive tok =
  let (tok_second, f1) = (parse_multi tok) in
  match tok_second with
  | Tok_Add :: t -> let tok_third = (match_token tok_second Tok_Add) in 
                      let (tok_for, f2) = (parse_additive tok_third) in 
                      (tok_for, Add(f1, f2))
  | Tok_Sub :: t -> let tok_third = (match_token tok_second Tok_Sub) in 
                         let (tok_for, f2) = (parse_additive tok_third) in 
                         (tok_for, Sub(f1, f2))
  | _ -> (tok_second, f1)

and parse_multi tok =
  let (tok_second, f1) = (parse_power tok) in
  match tok_second with
  | Tok_Mult :: t -> let tok_third = (match_token tok_second Tok_Mult) in 
                      let (tok_for, f2) = (parse_multi tok_third) in 
                      (tok_for, Mult(f1, f2))
  | Tok_Div :: t -> let tok_third = (match_token tok_second Tok_Div) in 
                         let (tok_for, f2) = (parse_multi tok_third) in 
                         (tok_for, Div(f1, f2))
  | _ -> (tok_second, f1)

and parse_power tok =
  let (tok_second, f1) = (parse_unary tok) in
  match tok_second with
  | Tok_Pow :: t -> let tok_third = (match_token tok_second Tok_Pow) in 
                      let (tok_for, f2) = (parse_power tok_third) in 
                      (tok_for, Pow(f1, f2))
  | _ -> (tok_second, f1)

and parse_unary tok =
  match tok with
  | Tok_Not :: t -> let tok_second = (match_token tok Tok_Not) in 
                      let (tok_third, f1) = (parse_unary tok_second) in 
                      (tok_third, Not(f1))
  | _ -> (parse_primary tok)

and parse_primary tok = 
  match tok with
  | Tok_Int(x) :: t -> let tok_second = (match_token tok (Tok_Int(x))) in 
                        (tok_second, Int(x))
  | Tok_Bool(x) :: t -> let tok_second = (match_token tok (Tok_Bool(x))) in (
                        tok_second, Bool x)
  | Tok_ID(x) :: t -> let tok_second = (match_token tok (Tok_ID(x))) in 
                        (tok_second, ID x)
  | Tok_LParen :: t -> let tok_second = (match_token tok Tok_LParen) in 
                       let (tok_third, f2) = (parse_expr tok_second) in 
                       match tok_third with
                      | Tok_RParen :: t -> (t, f2)
                      | _ -> raise (InvalidInputException ("wrong"))
  | _ -> let x = (List.hd tok) in raise(InvalidInputException("x"))
;;

                                      

let rec parse_stmt toks : stmt_result =
  match toks with 
  | Tok_Int_Type :: t -> let (tok_first, declare) = (declareStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(declare, stmt))

  | Tok_Bool_Type :: t -> let (tok_first, boolean) = (declareStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(boolean, stmt))     
                         
  | Tok_ID(x) :: t -> let (tok_first, assignment) = (assignStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(assignment, stmt))       

  | Tok_Print :: t -> let (tok_first, print) = (printStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(print, stmt))

  | Tok_If :: t -> let (tok_first, ifstatement) = (ifStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(ifstatement, stmt))     
                          
  | Tok_For :: t -> let (tok_first, forstatement) = (forStmt toks) in
                         let (tok_sec, stmt) = (parse_stmt tok_first) in 
                         (tok_sec, Seq(forstatement, stmt))        
  | Tok_While :: t -> let (tok_first, whilestatement) = (whileStmt toks) in 
                      let (tok_sec, stmt) = (parse_stmt tok_first) in 
                      (tok_sec, Seq(whilestatement, stmt))
  | _ -> (toks, NoOp)                    
  
and declareStmt tok = 
  match tok with
  | Tok_Int_Type :: t -> let tok_sec = match_token tok Tok_Int_Type in 
                         (match tok_sec with
                            | Tok_ID(x) :: t -> let tok_third = match_token tok_sec (Tok_ID(x)) in 
                                                let tok_for = match_token tok_third Tok_Semi in
                                                (tok_for, Declare(Int_Type, x))
                            | _ -> raise (InvalidInputException("int declare Error")))

  | Tok_Bool_Type :: t -> let tok_sec = match_token tok Tok_Bool_Type in 
                          (match tok_sec with
                            | Tok_ID(x) :: t -> let tok_third = match_token tok_sec (Tok_ID(x)) in 
                                                let tok_for = match_token tok_third Tok_Semi in
                                                (tok_for, Declare(Bool_Type, x))
                            | _ -> raise (InvalidInputException("bool declare Error")))

  | _ -> raise(InvalidInputException("theres nothing Error"))

and assignStmt tok = 
 match tok with
 | Tok_ID(x) :: t -> let tok_sec = match_token tok (Tok_ID(x)) in 
                     let tok_third = match_token tok_sec Tok_Assign in 
                     let (tok_for, expr) = (parse_expr tok_third) in 
                     let tok_five = match_token tok_for Tok_Semi in
                     (tok_five, Assign(x, expr))
 | _ -> raise(InvalidInputException("a Error"))

and printStmt tok = 
  match tok with 
  | Tok_Print :: t -> let tok_sec = match_token tok Tok_Print in 
                      let tok_third = match_token tok_sec Tok_LParen in 
                      let (tok_for, expr) = parse_expr tok_third in 
                      let tok_five = match_token tok_for Tok_RParen in 
                      let tok_six = match_token tok_five Tok_Semi in 
                      (tok_six, Print(expr))
  | _ -> raise(InvalidInputException("P Error"))

and ifStmt tok = 
  match tok with 
  | Tok_If :: t -> let tok_sec = match_token tok Tok_If in 
                   let tok_third = match_token tok_sec Tok_LParen in 
                   let (tok_for, ifexpr) = parse_expr tok_third in 
                   let tok_five = match_token tok_for Tok_RParen in 
                   let tok_six = match_token tok_five Tok_LBrace in 
                   let (tok_sev, ifstatement) = parse_stmt tok_six in 
                   let tok_eight = match_token tok_sev Tok_RBrace in 
                   match tok_eight with 
                   | Tok_Else :: t -> let tok_nine = match_token tok_eight Tok_Else in 
                                      let tok_ten = match_token tok_nine Tok_LBrace in 
                                      let (tok_ele, elsestate) = parse_stmt tok_ten in 
                                      let tok_twe = match_token tok_ele Tok_RBrace in 
                                      (tok_twe, If(ifexpr, ifstatement, elsestate))
                   | _ -> (tok_eight, If(ifexpr, ifstatement, NoOp))  
  | _ -> raise(InvalidInputException("if Error"))

 and forStmt tok = 
  match tok with 
  | Tok_For :: t -> let tok_sec = match_token tok Tok_For in
                    let tok_third = match_token tok_sec Tok_LParen in
                    let (tok_for, id) = parse_expr tok_third in
                    let tok_five = match_token tok_for Tok_From in
                    let (tok_six, expr1) = parse_expr tok_five in
                    let tok_sev = match_token tok_six Tok_To in
                    let (tok_eight, expr2) = parse_expr tok_sev in
                    let tok_nine = match_token tok_eight Tok_RParen in 
                    let tok_ten = match_token tok_nine Tok_LBrace in 
                    let (tok_ele, statement) = parse_stmt tok_ten in 
                    let tok_twe = match_token tok_ele Tok_RBrace in
                    match id with 
                    | ID x -> (tok_twe, For(x, expr1, expr2, statement))
                     
  | _ -> raise(InvalidInputException("for Error"))  

and whileStmt tok = 
  match tok with
  | Tok_While :: t -> let tok_sec = match_token tok Tok_While in 
                      let tok_third = match_token tok_sec Tok_LParen in 
                      let (tok_for, expr) = parse_expr tok_third in 
                      let tok_five = match_token tok_for Tok_RParen in
                      let tok_six = match_token tok_five Tok_LBrace in 
                      let (tok_sev, statement) = parse_stmt tok_six in 
                      let tok_eight = match_token tok_sev Tok_RBrace in 
                      (tok_eight, While(expr, statement))
  | _ -> raise(InvalidInputException("while error"))
;;

let parse_main toks : stmt =
  let tok = (match_token toks Tok_Int_Type) in 
  let tok_first = (match_token tok Tok_Main) in 
  let tok_sec = (match_token tok_first Tok_LParen) in 
  let tok_third = (match_token tok_sec Tok_RParen) in 
  let tok_for = (match_token tok_third Tok_LBrace) in
  let (tok_for, statement) = (parse_stmt tok_for) in 
  let tok_five = (match_token tok_for Tok_RBrace) in
  if lookahead tok_five = EOF then
    statement
  else raise(InvalidInputException("EOF missing"))
;;
