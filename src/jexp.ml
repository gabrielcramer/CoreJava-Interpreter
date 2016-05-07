open Core.Std
open Lexer
open Lexing
open Syntax
open Interpreter
open Exn2

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s: line = %d, column = %d at `%s`" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (Lexing.lexeme lexbuf)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error while parsing\n" print_position lexbuf;
    exit (-1)

let interpret lexbuf =
  match parse_with_error lexbuf with
  | Some program -> begin print_endline (Syntax.show_program program);
      try Typechecker.typeCheckProgram program;
        let _ = Interpreter.interpretProgram program in () with
      | StaticError msg -> print_endline ("Static error: " ^ msg)
      | RuntimeError msg -> print_endline ("Runtime error:" ^ msg)
    end
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  interpret lexbuf;
  In_channel.close inx


let () =
  Command.basic ~summary:"jexp - Interpreter for Java-like expression oriented language"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
