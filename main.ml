
type lexer_token_ty = TyOpenParan | TyCloseParen | TyEof | TyUnkown | TyHashtag | TySpace | TyNewLine | TyLiteral;;
type literal_types = LitString | LitInt;;
type literal_token_value = {
  ty: literal_types;
  literal_value: string;
}
type lexer_token = {
  ty: lexer_token_ty;
  tok_literal: literal_token_value option;
}

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

(*A completely useless function other than debugging*)
let tok_to_string lexer_token = 
  match lexer_token with
    | TyOpenParan -> "Open Parentheses -> ("
    | TyCloseParen -> "Close Parentheses ->)"
    | TyHashtag -> "Hashtag -> #"
    | TyEof -> "EOF"
    | TyLiteral -> "Literal"
    | _ -> "unkown"

class lexer input = object (self)
    val mutable ret = ([] : lexer_token list)
    val mutable idx = 0
    val input = input

    method lex =
    if idx > String.length input then
      ret <- { ty = TyEof; tok_literal = None }:: ret
    else 
      let next_char = String.get input idx  in  
      match next_char with 
      | '(' -> ret <- { ty = TyOpenParan; tok_literal = None }:: ret;
      | ')' -> ret <- { ty = TyCloseParen; tok_literal = None }:: ret;
      | '#' -> ret <- { ty = TyHashtag; tok_literal = None }:: ret;
      | ' ' -> ret <- { ty = TySpace; tok_literal = None }:: ret;
      | '\n' -> 
        print_string "perhaps you wanted a \\ instead of a newline...";
        ret <- { ty = TyNewLine; tok_literal = None } :: ret;
      | _ ->
        let literal_val = ref "" in
          let c = ref (String.get input idx) in
            (*literal := !literal ^ "s";*)
            while is_alpha !c || is_digit !c || !c == '_' do
              literal_val := !literal_val ^ String.make 1 !c;
              idx <- idx + 1;
              c := String.get input idx;
              ()
            done;
          print_string !literal_val;
          print_newline ();
          let literal_value = {
            ty = LitString;
            literal_value = !literal_val;
          } in 
            let literal_token = {
              ty = TyLiteral;
              tok_literal = Some literal_value;
            } in 
              ret <- literal_token:: ret;
    
    method lex_all = 
      while idx < String.length input do
        ignore(self#lex);
        idx <- idx + 1;
      done

    method get_ret = ret
  end;;

let open_up_file_and_use_results file_name = 
  let file = open_in_bin file_name in 
    let s = really_input_string file (in_channel_length file) in
    close_in file;
    s

let main = let lexer_instance = new lexer (open_up_file_and_use_results "definitions.shinyd") in
  lexer_instance#lex_all;
  (*print_int (List.length lexer_instance#get_ret);;*)

  let ret = lexer_instance#get_ret in
  let length = List.length ret in
  let idx = ref 0 in
    while !idx < length do
      let ele = List.nth ret !idx in
        if ele.ty == TyLiteral then
          match ele.tok_literal with
          | None -> print_string "You lied"
          | Some lit -> 
            print_string ("The literal value is " ^ lit.literal_value);
            print_newline ();
        else
          ()
          ;
      idx := !idx + 1;
      ()
    done