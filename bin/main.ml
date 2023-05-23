(* let context = Llvm.global_context ()
let llvm_void = Llvm.void_type context
let llvm_i32 = Llvm.i32_type context
let llvm_i8 = Llvm.i8_type context *)

exception ParserHitTheEnd;;
type std_types = String [@@deriving show];;
type expression_literals = StringLiteral of string [@@deriving show];;
type expression = 
  Empty
  | Definition of { 
      d_name: string;
      d_type_list: std_types list option;
      d_args_list: string list option; 
      d_body: expression; 
    } 
  | Block of expression list
  | Literal of expression_literals
  | Identifier of string
  | FunctionCall of {
    f_name: string;
    f_args: expression list option
  } [@@deriving show];;
type parser_result = 
  Expr of expression 
  | StdType of std_types [@@deriving show];;

type lexer_token_ty = TyFatArrow | TyOpenParan | TyString of string | TyAtSymbol | TyBackSlash | TyCloseParen | TyEof | TyHashtag | TySpace | TyLiteral | TyEq [@@deriving show];;
(*type literal_types = LitString | LitInt;;*)
type literal_token_value = {
  literal_value: string;
} [@@deriving show];;

type lexer_token = { 
  ty: lexer_token_ty; 
  tok_literal: literal_token_value option; 
} [@@deriving show];;


let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

(*The parse function is parse_expr don't let the name confuse you.*)
class parser tokens = object (self)
  val tokens = (tokens : lexer_token list)
  val ret = ([] : parser_result list)
  val mutable idx = 0

  method expect expected = 
    ignore(self#advance);
    self#expect_tty expected

  method expect_tty expected = 
    let exception ExpectedSomethingElse in
    let peeked = List.nth tokens idx in
      if peeked.ty != expected then
        let _ = print_endline (show_lexer_token peeked) in
          raise ExpectedSomethingElse

  method peek = 
    let exception AllOutOfTokens in
      if idx == 0 then
        List.nth tokens 0
      else
        if idx + 1 == List.length(tokens) then
          raise ParserHitTheEnd
        else
          if idx + 1 >= List.length(tokens) then
            raise AllOutOfTokens
          else
            List.nth tokens (idx + 1)

  method advance = 
    if idx + 1 >= List.length(tokens) then
      raise ParserHitTheEnd
    else
      idx <- idx + 1;
      let nth_tok = List.nth tokens idx in
        nth_tok

  method parse_all = 
    while idx != List.length(tokens) do
      ignore(self#parse_expr);
    done

  method parse_block block =
    if idx < List.length tokens then
      let current = List.nth tokens idx in
        block := Result.get_ok (self#parse_expr) :: !block;
        if current.ty == TyCloseParen then
          self#parse_block block;

  method parse_expr =
    let exception UnknownToken in
    let next = self#peek in
      match next.ty with
        | TyBackSlash -> 
          ignore(self#advance);
          ignore(self#advance);
          print_endline "Backslash";
          let backlash_lit = self#parse_literal in
            Ok (Identifier backlash_lit.literal_value)
        | TyString str ->
          ignore(self#advance);
          Ok (Literal (StringLiteral str))
        | TyAtSymbol ->
          let f_name = self#parse_literal in
          let f_args = ref ([]: expression list) in 
            while idx < List.length tokens do
              let expr = Result.get_ok self#parse_expr in
                ignore(self#advance);
                f_args := expr :: !f_args;
            done;
            ignore(self#advance);
            Ok (FunctionCall { f_name = f_name.literal_value; f_args = Some !f_args })
        | TyOpenParan -> 
          let next_peek = self#advance in
            (match next_peek.ty with
            (*Parse definition*)
            | TyLiteral ->
              self#parse_definition;
              Ok(Empty)
            | TyAtSymbol ->
              Ok(Empty)
            | _ -> 
              let block = ref ([]: expression list) in
                self#parse_block block;
                ignore(self#advance);
                Ok (Block !block))
        | TyLiteral -> 
          ignore(self#advance);
          Ok (Identifier self#parse_literal.literal_value)
        | TyEof -> print_endline("1"); 
          Error ParserHitTheEnd
        | _ -> 
          print_endline (Printf.sprintf "2 = %s" (show_lexer_token next));
          ignore(self#advance);
          Error UnknownToken
        ;
          (*(@print (@print_this "Hello, World!"))*)
  method parse_literal =
    let exception ExpectedLiteral in
    let supposeed_to_be_literal = List.nth tokens idx in
      if supposeed_to_be_literal.ty != TyLiteral then
        let _ = print_endline (show_lexer_token supposeed_to_be_literal) in
        raise ExpectedLiteral
      else
        match supposeed_to_be_literal.tok_literal with
        | Some lit ->  lit
        | _ -> raise ExpectedLiteral

  method parse_call = 
    ignore(self#advance);
    ()

  method parse_definition = 
    let lhs = self#parse_literal in
    print_endline (Printf.sprintf "Literal value of definition is %s" lhs.literal_value);
    let def_types = ref ([] : expression list) in
      while List.mem self#peek.ty [TyFatArrow] == false do
        let expr = self#parse_expr in
        def_types := Result.get_ok expr :: !def_types;
        ()
      done;
      
    print_endline (Printf.sprintf "\n%s type list is %d long\n" lhs.literal_value (List.length !def_types));
    (* if List.length !def_types > 1 then
      Curry *)
    ignore(self#expect TyFatArrow);
    print_endline (Printf.sprintf "\nValue of definition %s is %s" lhs.literal_value (show_expression (Result.get_ok self#parse_expr)));
    ignore(self#expect TyCloseParen);
    print_endline "End Definiton";

  (*Starts parsing under the assumption the current token is a hashtag*)
  method parse_derective =
    let exception ExpectedLiteral in
      (* Get rid of the hashtag cause at this point we don't care about it *)
      let supposeed_to_be_literal = self#advance in
        if supposeed_to_be_literal.ty != TyLiteral then
          raise ExpectedLiteral
        else
          match supposeed_to_be_literal.tok_literal with
            | Some lit -> print_endline lit.literal_value
            | _ -> raise ExpectedLiteral
          ;
        end;;

class lexer input = object (self)
    val mutable ret = ([] : lexer_token list)
    val mutable idx = 0
    val input = input

    method lex =
      let exception LeaveMeAlone in
      try
        if idx > String.length input then
          raise LeaveMeAlone
        else
          let next_char = String.get input idx in  
            match next_char with 
            | '(' -> self#push_back TyOpenParan
            | ')' -> 
              print_endline "2";
              self#push_back TyCloseParen
            | '#' -> self#push_back TyHashtag
            | '@' -> self#push_back TyAtSymbol
            | '\\' -> print_endline "Backslash";self#push_back TyBackSlash
            | '"' ->
              ignore(self#advance);
              let exception EarlyRetreat in
                let str_val = ref (String.make 1 (String.get input idx)) in
                try while idx < String.length input do
                  let c = String.get input (idx + 1) in
                    if c == '"' then
                      let _ = self#advance in
                      raise EarlyRetreat
                    else
                      str_val := !str_val ^ Char.escaped c;
                      self#advance;
                done
                with EarlyRetreat -> self#push_back (TyString !str_val);
              ;
                (*ret <- { ty = TyString; tok_literal = None } :: ret;*)
            | '=' -> 
              let next = String.get input (idx +1) in
                ignore(self#advance);
                match next with
                  | '>' -> self#push_back TyFatArrow;
                  | _ -> self#push_back TyEq;
                ;
            | ' ' | '\n' -> ignore(self#advance);
            | _ ->
              let literal_val = ref (String.make 1 next_char) in
              let exception EarlyRetreat in
                try while idx < String.length input do
                  let c = String.get input (idx + 1) in
                    if is_alpha c || is_digit c || c == '_' then
                      let _ = self#advance in
                        literal_val := !literal_val ^ (String.make 1 c);
                    else
                      let _ = print_endline (Printf.sprintf "'%s' Failed on '%c'" !literal_val c) in
                        raise EarlyRetreat;
                  done
              with EarlyRetreat -> 
                print_endline !literal_val;
                let literal_value = {
                  literal_value = !literal_val;
                } in 
                  let literal_token = {
                    ty = TyLiteral;
                    tok_literal = Some literal_value;
                  } in 
                    ret <- literal_token:: ret;
                    ignore(self#advance);
                    print_endline (Printf.sprintf "Whilst inserting '%s' we ended with %c" !literal_val (String.get input idx));
              ;
      with
        LeaveMeAlone -> ()

    method advance = 
      idx <- idx + 1;

    method push_back ty = 
      ret <- { ty = ty; tok_literal = None} :: ret;
      ignore(self#advance);

    method lex_all = 
      while idx != String.length input do
        ignore(self#lex);
      done;
      print_endline "Done";
    
    method get_ret = ret
  end;;

let open_up_file_and_use_results file_name = 
  let file = open_in_bin file_name in 
    let s = really_input_string file (in_channel_length file) in
    close_in file;
    s

(* let () = 
  let builder = Llvm.builder context in
  let main_module = Llvm.create_module context "main" in

  let main_fn_arg_tys = Array.make 0 llvm_void in
  let print_ty = Llvm.var_arg_function_type llvm_i32 main_fn_arg_tys in
  let print_fn = Llvm.declare_function "printf" print_ty main_module in
  let main_ty = Llvm.function_type llvm_void main_fn_arg_tys in
  let main_fn = Llvm.declare_function "main" main_ty main_module in
  let fn_entry = Llvm.append_block context "entry" main_fn in 
  let to_print = "Hello, World\\0" in
  let to_print_len = String.length(to_print) in
  let string_ty = Llvm.array_type llvm_i8 to_print_len in
  let _ = Llvm.position_at_end fn_entry builder in
  let alloc_val = Llvm.const_string context to_print in
  let alloc = Llvm.build_alloca string_ty "to_print" builder in
  let _ = Llvm.build_store alloc_val alloc builder in
  let variable_array = Array.make 1 alloc in
  let _ = Llvm.build_call print_fn variable_array "call_print_f" builder in
  let _ = Llvm.position_at_end fn_entry builder in
  let _ = Llvm.build_ret_void builder in
  let _ = Llvm.print_module "out.ll" main_module in
    () *)

let () = 
  let lexer_instance = new lexer (open_up_file_and_use_results "examples/normal.shiny") in
    lexer_instance#lex_all;
    (*print_int (List.length lexer_instance#get_ret);;*)
    let ret = lexer_instance#get_ret in
    (* let whatever (lt:lexer_token) = print_endline (show_lexer_token lt); true in
    let _ = List.for_all whatever (List.rev ret) in
    let _ = print_endline "\n" in *)
    let parser = new parser (List.rev ret) in
      parser#parse_all;

    ()
    