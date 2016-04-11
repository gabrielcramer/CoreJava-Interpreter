open Core.Std
open Lexer
open Lexing
open Syntax
open Interpreter

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec print_fields fields =
  match fields with
  | []-> "\n"
  | hd :: tl -> match hd with
    |(IntType,id) -> "int " ^ id ^ ";\n" ^ print_fields tl
    |(FloatType,id) -> "float " ^ id ^ ";\n" ^ print_fields tl
    |(BoolType,id) -> "bool " ^ id ^ ";\n" ^ print_fields tl
    |(VoidType,id) -> "void " ^ id ^ ";\n" ^ print_fields tl
    |(ObjectType(objType),id) -> objType ^ id ^ ";\n" ^ print_fields tl


let rec printClassList = function
    [Class(a,b,fields,_)] -> print_string ("class " ^ a ^ " extends " ^ b ^ "{\n"^ (print_fields fields) ^ "#\n" ^ "}" ^ "\n");
  |Class(a,b,fields,_) :: tl -> print_string ("class " ^ a ^ " extends " ^ b ^ "{\n"^ (print_fields fields) ^ "#\n" ^ "}" ^ "\n"); printClassList tl;;
let printProgram = function
    Program classlist -> printClassList classlist;;


let printValue = function
    NullV -> print_string("NullV\n");
  |IntV(i) -> print_string("Int " ^ (string_of_int i) ^ "\n");
  |FloatV(f) -> print_string("Float " ^ (string_of_float f) ^ "\n");
  |BoolV(b) -> print_string("Bool " ^ (string_of_bool b) ^ "\n");
  |VoidV -> print_string("Void\n");
  |LocV _ -> print_string ("Location\n");;

let printExp = function
    Value(v) -> (printValue v);
  |Operation(_,_,_) -> print_string("Main method has an operation as final exp.\n");
  | _ -> print_string("Other type\n");;

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  |Some program-> printProgram program
  |None -> printExp (Value(IntV (2)));;

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx


let () =
  Command.basic ~summary:"Test CoreJava parser"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
