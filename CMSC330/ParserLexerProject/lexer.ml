open TokenTypes

let tokenize input =
  let boolofbool = Str.regexp "true\\|false" in 
  let intofint = Str.regexp "[-]?[0-9]+" in
  let id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in

  let lparen = Str.regexp "(" in
  let rparen = Str.regexp ")" in 
  let lbrace = Str.regexp "{" in
  let rbrace = Str.regexp "}" in
  let equal = Str.regexp "==" in
  let notequal = Str.regexp "!=" in
  let assign = Str.regexp "=" in
  let greater = Str.regexp ">" in
  let less = Str.regexp "<" in
  let greaterequal = Str.regexp ">=" in
  let lessequal = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let not = Str.regexp "!" in
  let semi = Str.regexp ";" in
  let inttype = Str.regexp "int" in
  let booltype = Str.regexp "bool" in
  let print = Str.regexp "printf" in
  let main = Str.regexp "main" in
  let re_if = Str.regexp "if" in
  let re_else = Str.regexp "else" in
  let re_for = Str.regexp "for" in
  let re_from = Str.regexp "from" in
  let re_to = Str.regexp "to" in
  let re_while = Str.regexp "while" in
  let add = Str.regexp "+" in
  let sub = Str.regexp "-" in
  let multi = Str.regexp "*" in
  let div = Str.regexp "/" in
  let power = Str.regexp "\\^" in

  let whitespace = Str.regexp "[ \t\n]*" in
  let extraspace = Str.regexp "[a-zA-Z0-9]+" in

  let rec check str pos = 
    if pos >= (String.length str) then [EOF]

    else if (Str.string_match lparen str pos) then (Tok_LParen) :: (check str (pos + 1)) 
    else if (Str.string_match rparen str pos) then (Tok_RParen) :: (check str (pos + 1)) 
    else if (Str.string_match lbrace str pos) then (Tok_LBrace) :: (check str (pos + 1)) 
    else if (Str.string_match rbrace str pos) then (Tok_RBrace) :: (check str (pos + 1)) 
    else if (Str.string_match equal str pos) then (Tok_Equal) :: (check str (pos + 2)) 
    else if (Str.string_match greaterequal str pos) then (Tok_GreaterEqual) :: (check str (pos + 2)) 
    else if (Str.string_match notequal str pos) then (Tok_NotEqual) :: (check str (pos + 2)) 
    else if (Str.string_match lessequal str pos) then (Tok_LessEqual) :: (check str (pos + 2)) 
    else if (Str.string_match assign str pos) then (Tok_Assign) :: (check str (pos+1))
    else if (Str.string_match greater str pos) then (Tok_Greater) :: (check str (pos+1))
    else if (Str.string_match less str pos) then (Tok_Less) :: (check str (pos+1))
    else if (Str.string_match re_or str pos) then (Tok_Or) :: (check str (pos+2))
    else if (Str.string_match re_and str pos) then (Tok_And) :: (check str (pos+2))
    else if (Str.string_match not str pos) then (Tok_Not) :: (check str (pos+1))
    else if (Str.string_match semi str pos) then (Tok_Semi) :: (check str (pos+1))
    else if (Str.string_match inttype str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_Int_Type) :: (check str token_pos)
    else if (Str.string_match booltype str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_Bool_Type) :: (check str token_pos)
    else if (Str.string_match print str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_Print) :: (check str token_pos)
    else if (Str.string_match main str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_Main) :: (check str token_pos)
    else if (Str.string_match re_if str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_If) :: (check str token_pos)
    else if (Str.string_match re_else str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_Else) :: (check str token_pos)
    else if (Str.string_match re_while str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_While) :: (check str token_pos)
    else if (Str.string_match re_from str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_From) :: (check str token_pos)
    else if (Str.string_match re_for str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_For) :: (check str token_pos)
    else if (Str.string_match re_to str pos) then 
      let long = Str.matched_string str in 
      let token_pos = Str.match_end() in
      if (Str.string_match extraspace str token_pos) then
        let word = Str.matched_string str in 
        (Tok_ID (long ^ word)) :: (check str (Str.match_end()))
      else (Tok_To) :: (check str token_pos)
    else if (Str.string_match add str pos) then (Tok_Add) :: (check str (pos+1))
    else if (Str.string_match sub str pos) then 
      let token_pos = Str.match_end() in
      if (Str.string_match intofint str token_pos) then
        let num = Str.matched_string str in
        let temp = int_of_string(num) in
        let neg = temp * -1 in 
        (Tok_Int (neg)) :: (check str (Str.match_end())) 
      else (Tok_Sub) :: (check str (pos+1))
    else if (Str.string_match div str pos) then (Tok_Div) :: (check str (pos+1))
    else if (Str.string_match multi str pos) then (Tok_Mult) :: (check str (pos+1))
    else if (Str.string_match power str pos) then (Tok_Pow) :: (check str (pos+1))
    else if (Str.string_match boolofbool str pos) then 
      let tok = Str.matched_string str in
      (Tok_Bool (bool_of_string tok)) :: (check str (Str.match_end()))
    else if (Str.string_match intofint str pos) then 
      let tok = Str.matched_string str in
      (Tok_Int (int_of_string tok)) :: (check str (Str.match_end()))
    else if (Str.string_match id str pos) then 
      let tok = Str.matched_string str in
      (Tok_ID tok) :: (check str (Str.match_end()))
    else if (Str.string_match whitespace str pos) then (check str (Str.match_end())) 
    else failwith "No"
  in
  check input 0
;;  
