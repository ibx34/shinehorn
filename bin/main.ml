(* let context = Llvm.global_context ()
let llvm_void = Llvm.void_type context
let llvm_i32 = Llvm.i32_type context
let llvm_i8 = Llvm.i8_type context *)

exception ParserHitTheEnd;;
exception UnexpectedToken;;
exception ExpectedSomethingElse;;
exception ExpectedLiteral;;
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

  (*Checks if the current element is equal to the expectant*)
  (*TODO: add function that peeks ahead and checks*)
  method expect expectant =
    let next = List.nth tokens idx in
      if next.ty != expectant then
          let _ = print_endline (Printf.sprintf "%s != %s" (show_lexer_token_ty next.ty) (show_lexer_token_ty expectant)) in
          raise ExpectedSomethingElse
      else
        ignore(self#advance 1);

  (*Advances the parser by advance_by_amount*)
  method advance advance_by_amount = 
    if idx + advance_by_amount >= List.length tokens then
      raise ParserHitTheEnd
    else
      idx <- idx + advance_by_amount;

  (*Advances the parser by advance_by_amount and then returns the token at the NEW index*)
  method advance_ret advance_by_amount =
    let _ =  self#advance advance_by_amount in
    List.nth tokens idx
  (*(@print (@print_this "Hello, World!"))*)
  (*Looks forward in the lexer_tokens list by peek_by and returns the token at the new index*)
  method peek peek_by = 
    if idx + peek_by >= List.length tokens then
      raise ParserHitTheEnd
    else
      List.nth tokens (idx + peek_by)

  method parse_literal = 
    let supposeed_to_be_literal = List.nth tokens idx in
    if supposeed_to_be_literal.ty != TyLiteral then
      raise ExpectedLiteral
    else
      match supposeed_to_be_literal.tok_literal with
      | Some lit -> 
        ignore(self#advance 1);
        lit
      | _ -> raise ExpectedLiteral

  method parse_definition = 
    let definition_name = self#parse_literal in
    print_endline (Printf.sprintf "The name of the definition is: \"%s\"" definition_name.literal_value);
    let type_list = ref ([] : expression list) in
    while List.mem (List.nth tokens idx).ty [TyFatArrow] == false do
      let expr = self#parse_expr in
      type_list := Result.get_ok expr :: !type_list;
      ()
    done
    ;
    let whatever (lt:expression) = print_endline (show_expression lt); true in
    let _ = List.for_all whatever (List.rev !type_list) in
    ignore(self#expect TyFatArrow);
    let rhs = self#parse_expr in
    print_endline (Printf.sprintf "The value of the definition is:\n%s\n*******" (show_expression (Result.get_ok rhs)));
    print_endline "\n"
  
  method parse_expr = 
    let current = List.nth tokens idx in
    match current.ty with
      | TyLiteral ->
        Ok (Identifier self#parse_literal.literal_value)
      | TyBackSlash -> 
        let _ = self#advance 1 in
        let backlash_lit = self#parse_literal in
          Ok (Identifier backlash_lit.literal_value)
          
      | TyOpenParan -> 
        print_endline "Open Paren";
        let next = self#advance_ret 1 in
        (match next.ty with
          | TyLiteral -> 
            let _ = self#parse_definition in
            Ok (Empty) 
          | _ -> Error UnexpectedToken
        )
      | _ -> Error UnexpectedToken
    ;

  method parse_all = 
    while (List.nth tokens idx).ty != TyEof do
      ignore(self#parse_expr);
      ignore(self#advance 1);
    done

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
    