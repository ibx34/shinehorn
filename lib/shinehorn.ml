module Common = struct 
  exception ParserHitTheEnd [@@deriving show];;
  exception UnexpectedToken [@@deriving show];;
  exception ExpectedSomethingElse [@@deriving show];;
  exception ExpectedLiteral [@@deriving show];;
  type std_types = String [@@deriving show];;
  type expression_literals = StringLiteral of string [@@deriving show];;
  type expression = 
    | Empty
    | DefinitionTypeListItem of {
      ident: expression option;
      _type: string;
    }
    | Definition of { 
        d_name: string;
        (*TODO: This should be one of std_types but im a bit lazy :()*)
        d_type_list: expression list;
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

  type lexer_token_ty = TyLCurlyBracket | TyRCurlyBracket | TyFatArrow | TyOpenParan | TyString of string | TyAtSymbol | TyBackSlash | TyCloseParen | TyEof | TyHashtag | TySpace | TyLiteral | TyEq [@@deriving show];;
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
end

module LLVMFront = struct
  exception ExpecctedDifferentType [@@deriving show];;
  exception NotAFunctionDef [@@deriving show];;
  exception UndefinedType [@@deriving show];;
  let context = Llvm.global_context ()
  let llvm_void = Llvm.void_type context
  let llvm_i32 = Llvm.i32_type context
  let llvm_i8 = Llvm.i8_type context
  let llvm_i64 = Llvm.i64_type context
  let llvm_void_array = Array.make 0 llvm_void
  let llvm_string_type size = Llvm.array_type llvm_i8 size
  (*All functions in the llvm object should return this so its easier
    to handle the definitionn of functions and first class citizens:)*)
  type type_wrappers = 
  Llvm of Llvm.lltype
  | Fn of { name: string; args: type_wrappers list option; ret: type_wrappers option; body: Common.expression}

  (*Should i have my own object type for containing stuff? I dont know right now...*) 
  class llvm input = object (self)
    val parser_results_or_ast = (input : Common.parser_result list)
    val mutable idx = 0
    (*10 is arbitrary and doesnt really matter. it can be expanded for faster inserts i think?*)
    val mutable functions = Hashtbl.create 10
    val mutable mods = Hashtbl.create 10
    val mutable custom_types = Hashtbl.create 10

    method advance advance_by_amount = 
      if idx + advance_by_amount <= List.length parser_results_or_ast then
        idx <- idx + advance_by_amount;

    (*Crates the main module*)
    method init =
      let main = Llvm.create_module context "main" in
        let print_ty = Llvm.var_arg_function_type llvm_i32 llvm_void_array in
        (*save*)
        let print_fn = Llvm.declare_function "printf" print_ty main in
        (*Both this and the modules list should be some sorta of python dict-esk thing so we can easily 
        look up values*)
        Hashtbl.add functions "c_print" print_fn;
        Hashtbl.add mods "main" main;
  
        let struct_types = (Array.make 1 llvm_i8) in
        let struct_types = Array.append struct_types (Array.make 1 llvm_i64) in
        (*This creates a named struct.*)
        let struct_ty = Llvm.named_struct_type context "String" in
        let _ = Llvm.struct_set_body struct_ty struct_types false in
        Hashtbl.add custom_types "string" struct_ty;
        (* let struct_vals = Array.make 1 (Llvm.const_int llvm_i8 5) in 
        let const_struct_value = Llvm.const_named_struct struct_ty struct_vals in
        let _ = Llvm.define_global "random_struct" const_struct_value main_module in *)
      
    method parse = function 
      | Common.Expr (Common.Definition (_)) as def -> self#parse_definition def
      | _ -> ()

    method parse_definition = function
      | Common.Expr (Common.Definition { 
        d_name: string;
        d_type_list: Common.expression list;
        d_body: Common.expression; 
      } ) -> let named_args = List.filter (fun x -> (match x with
          | Common.DefinitionTypeListItem {
            ident: Common.expression option;
            _type: string;
          } -> Option.is_some ident
          | _ -> failwith "Somehow, this slipped in... it probably isnt handled rightnow"
        )) d_type_list in
          (*We need to walk the body of the expression and check for it's type. this can be done by making a generalized parse fn that returns the wrapped type *type from above*)
          print_endline (Common.show_expression d_body)
      | _ -> ()

    (*
    
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
    
    *)
  end;;
end