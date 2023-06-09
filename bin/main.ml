let context = Llvm.create_context ()
let llvm_void = Llvm.void_type context
let llvm_i32 = Llvm.i32_type context
let llvm_i8 = Llvm.i8_type context
let llvm_i64 = Llvm.i64_type context

open Shinehorn;;
open Shinehorn.Common;;

(*The parse function is parse_expr don't let the name confuse you.*)
class parser tokens = object (self)
  val tokens = (tokens : lexer_token list)
  val mutable ret = ([] : parser_result list)
  val mutable idx = 0
  (*If someone wants to tell me why i have to do this pls do*)
  (*I think it has to do with how in a block it doesnt advance after every expression is parsed
    whereas when ever parsing the entire file it does*)
  val mutable in_block = false

  method __expect expectant = 
    let next = List.nth tokens idx in
      next.ty == expectant

  (*Checks if the current element is equal to the expectant*)
  (*TODO: add function that peeks ahead and checks*)
  method expect expectant =
    if self#__expect expectant == false then
        raise ExpectedSomethingElse
    else
      ignore(self#advance 1);

  (*Advances the parser by advance_by_amount*)
  method advance advance_by_amount = 
    if idx + advance_by_amount <= List.length tokens then
      idx <- idx + advance_by_amount;

  (*Advances the parser by advance_by_amount and then returns the token at the NEW index*)
  method advance_ret advance_by_amount =
    let _ =  self#advance advance_by_amount in
    List.nth tokens idx
  (*Looks forward in the lexer_tokens list by peek_by and returns the token at the new index*)
  method peek peek_by = 
    if idx + peek_by == List.length tokens then
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
    let lhs = self#parse_literal in
    let type_list = ref ([] : expression list) in
    while List.mem (List.nth tokens idx).ty [TyFatArrow] == false do
      let expr = self#parse_expr in
      type_list := Result.get_ok expr :: !type_list;
      ()
    done
    ;
    let _ = ignore(self#expect TyFatArrow) in
    let rhs = self#parse_expr in
    if in_block then
      ignore(self#expect TyCloseParen);
    Ok (Definition ({ d_name = lhs.literal_value; d_body = Result.get_ok rhs; d_type_list = !type_list }))

  method parse_function_call = 
    (*Advance once to get past the @ symbol*)
    ignore(self#advance 1);
    let callee = self#parse_literal in
    print_endline (Printf.sprintf "Function being called: %s" callee.literal_value);

    let lhs = ref ([] : expression list) in
    while List.mem (List.nth tokens idx).ty [TyCloseParen] == false do
      let expr = self#parse_expr in
      lhs := Result.get_ok expr :: !lhs;
      ()
    done
    ;
    ignore(self#expect TyCloseParen);
    Ok (FunctionCall { f_name = callee.literal_value; f_args = Some !lhs })
    
  method parse_expr = 
    let current = List.nth tokens idx in
    match current.ty with
      | TyLiteral ->
        let literal = self#parse_literal in
        (*This is current because parse_literal advances once. 
            So it goes: See x (self#parse_literal) -> Parse -> Advance -> Check the next character*)
        let current = List.nth tokens idx in
        (match current.ty with
          | TyBackSlash -> let backlash_lit = Result.get_ok self#parse_expr in (match backlash_lit with
            | DefinitionTypeListItem dtli ->
                Ok (DefinitionTypeListItem { _type = dtli._type; ident = Some (Identifier literal.literal_value) })
            | _ -> raise ExpectedSomethingElse)
          | _ -> Ok (Identifier literal.literal_value) 
        )
      | TyBackSlash -> 
        let _ = self#advance 1 in
        let backlash_lit = self#parse_literal in
          Ok (DefinitionTypeListItem { _type = backlash_lit.literal_value; ident = None })
      | TyString str ->
        ignore(self#advance 1);
        Ok (Literal (StringLiteral str))
      | TyOpenParan -> 
        let next = self#advance_ret 1 in
        (match next.ty with
          | TyAtSymbol -> self#parse_function_call
          | TyLiteral -> self#parse_definition
          (*Block?*)
          | _ -> Error UnexpectedToken
        )
      | TyLCurlyBracket ->
        ignore(self#advance_ret 1);
        in_block <- true;
        let block = ref ([] : expression list) in
            while List.mem (List.nth tokens idx).ty [TyRCurlyBracket] == false do
              let expr = self#parse_expr in
              block := Result.get_ok expr :: !block;
              ()
            done
            ;
            ignore(self#expect TyRCurlyBracket);
            in_block <- false;
            Ok (Block !block)
      | _ -> print_endline (Printf.sprintf "2@: %s\n" (show_lexer_token current)); Error UnexpectedToken
    ;

  method parse_all = 
    (* try *)
      while idx != List.length tokens do
        let expr = self#parse_expr in
        let _ = ret <- Expr (Result.get_ok expr) :: ret in
        ignore(self#advance 1);
      done
      ;
      let whatever (lt:parser_result) = print_endline (show_parser_result lt); true in
        let _ = List.for_all whatever (List.rev ret) in
        print_endline (Printf.sprintf "\nLength: %d" (List.length ret));

  method get_ret = ret
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
            | '{' -> self#push_back TyLCurlyBracket
            | '}' -> self#push_back TyRCurlyBracket
            | ')' -> self#push_back TyCloseParen
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
            | ' ' | '\n' | '\t' -> ignore(self#advance);
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
  let main_module = Llvm.create_module context "main" in
  (*the i8 is the pointer and i64 is the length*)
  let struct_types = (Array.make 1 llvm_i8) in
  let struct_types = Array.append struct_types (Array.make 1 llvm_i64) in
  (*This creates a named struct.*)
  let struct_ty = Llvm.named_struct_type context "String" in
  let _ = Llvm.struct_set_body struct_ty struct_types false in
  let str_struct = Llvm.string_of_lltype struct_ty in
  print_endline str_struct;
  let struct_vals = Array.make 1 (Llvm.const_int llvm_i8 5) in 
  let const_struct_value = Llvm.const_named_struct struct_ty struct_vals in
  let _ = Llvm.define_global "random_struct" const_struct_value main_module in
  (* let main_fn_arg_tys = Array.make 0 llvm_void in
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
  let _ = Llvm.build_ret_void builder in *)
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
    let pret = List.rev parser#get_ret in
    let llvm = new LLVMFront.llvm (pret) in
      llvm#init;
      llvm#render (List.hd pret);
      (* llvm#handle_all;
      llvm#finished; *)
      ;
    ()
    