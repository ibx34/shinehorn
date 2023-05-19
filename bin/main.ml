(* let context = Llvm.global_context ()
let llvm_void = Llvm.void_type context
let llvm_i32 = Llvm.i32_type context
let llvm_i8 = Llvm.i8_type context *)

type lexer_token_ty = TyOpenParan | TyString | TyAtSymbol | TyBackSlash | TyCloseParen | TyEof | TyHashtag | TySpace | TyLiteral | TyEq [@@deriving show];;
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

class parser tokens = object (self)
  val tokens = (tokens : lexer_token list)
  val mutable idx = 0

  method peek = 
    List.nth tokens (idx + 1)

  method advance = 
    idx <- idx + 1;
    let nth_tok = List.nth tokens idx in
      (* Printf.printf "%s\n" (show_lexer_token nth_tok); *)
      nth_tok
    (* List.nth tokens idx *)

  method parse_all = 
    while idx < List.length tokens do
      (* Printf.printf "%s\n" (show_lexer_token(List.nth tokens idx)); *)
      ignore(self#parse);
    done

  method parse_expr = 
    let exception FailedToParse in
    let next = self#advance in
      match next.ty with
        | TyBackSlash -> self#parse_literal
        | _ -> raise FailedToParse
      ;

  method parse_literal =
    let exception ExpectedLiteral in
    let supposeed_to_be_literal = self#advance in
      if supposeed_to_be_literal.ty != TyLiteral then
        raise ExpectedLiteral
      else
        match supposeed_to_be_literal.tok_literal with
        | Some lit -> lit
        | _ -> raise ExpectedLiteral

  method parse_definition = 
    let exception ExpectedLiteral in
    let supposeed_to_be_literal = self#advance in
      if supposeed_to_be_literal.ty != TyLiteral then
        raise ExpectedLiteral
      else
        let literal = match supposeed_to_be_literal.tok_literal with
        | Some lit -> lit
        | _ -> raise ExpectedLiteral in
        
    let def_types = ref ([] : string list) in
    while List.mem self#peek.ty [TyEq] == false do
      let expr = self#parse_expr in
      def_types := expr.literal_value :: !def_types;
      ()
    done;

    print_endline (Printf.sprintf "\n%s type list is %d long\n" literal.literal_value (List.length !def_types))
    
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

  method parse =
    let exception LeaveMeAlone in
    try
      if idx > List.length tokens then
        raise LeaveMeAlone
      else
        let nt = List.nth tokens idx in
          match nt.ty with
            (*Parse expression*)
            | TyOpenParan -> let next_peek = self#peek in
              match next_peek.ty with
              (*Parse definition*)
              | TyLiteral -> self#parse_definition
              | _ -> ignore(self#advance);
              ;
            | _ -> ignore(self#advance);
    with
      LeaveMeAlone -> ()
  end

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
      | '@' -> ret <- { ty = TyAtSymbol; tok_literal = None } :: ret;
      | ' ' -> ret <- { ty = TySpace; tok_literal = None }:: ret;
      | '\\' -> ret <- { ty = TyBackSlash; tok_literal = None } :: ret;
      | '"' -> ret <- { ty = TyString; tok_literal = None } :: ret;
      | '=' -> ret <- { ty = TyEq; tok_literal = None } :: ret;
      | '/' -> let exception EarlyRetreat in
          try while idx < String.length input do
              let current = String.get input idx in
                if current == '\n' then
                  raise EarlyRetreat;
                  idx <- idx + 1;
            done
          with EarlyRetreat -> (); 
          ;
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

          let literal_value = {
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
    let parser = new parser (List.rev ret) in
      parser#parse_all;
    ()
    