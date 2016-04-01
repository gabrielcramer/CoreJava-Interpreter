open Core.Std
open Lexer
open Lexing
open AbstractSyntaxTree

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
                |Field(IntType,id) -> "int " ^ id ^ ";\n" ^ print_fields tl
                |Field(FloatType,id) -> "float " ^ id ^ ";\n" ^ print_fields tl
                |Field(BoolType,id) -> "bool " ^ id ^ ";\n" ^ print_fields tl
                |Field(VoidType,id) -> "void " ^ id ^ ";\n" ^ print_fields tl
                |Field(ObjectType(objType),id) -> objType ^ id ^ ";\n" ^ print_fields tl


let printProgram x = match x with
|Program[Class(a,b,fields,_);_]->
      print_string ("Program: class " ^ a ^ " extends " ^ b ^ "{\n"^ (print_fields fields) ^ "#\n" ^ "}" ^ "\n")
| _ -> print_string "No match\n";;


let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  |Some program-> printProgram program;
  |None -> ()

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
