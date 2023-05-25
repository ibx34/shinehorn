module Common = struct 
exception ParserHitTheEnd [@@deriving show];;
exception UnexpectedToken [@@deriving show];;
exception ExpectedSomethingElse [@@deriving show];;
exception ExpectedLiteral [@@deriving show];;
type std_types = String [@@deriving show];;
type expression_literals = StringLiteral of string [@@deriving show];;
type expression = 
  Empty
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
  let llvm_void_array = Array.make 0 llvm_void
  let llvm_string_type size = Llvm.array_type llvm_i8 size 
  (*Should i have my own object type for containing stuff? I dont know right now...*) 
  class llvm input = object (self)
    val parser_results_or_ast = (input : Common.parser_result list)
    val mutable idx = 0
    (*10 is arbitrary and doesnt really matter. it can be expanded for faster inserts i think?*)
    val mutable functions = Hashtbl.create 10
    val mutable mods = Hashtbl.create 10

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
    
    method create_function args_type = 
      let args_type_list = ref ([] : Llvm.lltype list) in
      let _ = List.for_all (fun ty -> 
      match ty with
      | Common.Identifier ident -> (match ident with 
        | "I32" -> args_type_list := llvm_i32 :: !args_type_list;
        | "I8" -> args_type_list := llvm_i8 :: !args_type_list;
        | _ -> raise UndefinedType);
        true
      | _ -> raise ExpecctedDifferentType
      ) args_type in
      ()

    method handle_definition ( def : Common.expression ) =
      match def with
        | Common.Definition d_data -> 
          if List.length d_data.d_type_list > 1 then
            let _ = self#create_function d_data.d_type_list in
            Ok "hi"
          else
            Ok "h2"
        | _ -> Error ExpecctedDifferentType
  end;;
end