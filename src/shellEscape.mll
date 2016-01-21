
{

open Buffer

}

rule unescape_lexer buf = parse
 | "\\\\" { add_char buf '\\'; unescape_lexer buf lexbuf }
 | "\\$" { add_char buf '$'; unescape_lexer buf lexbuf }
 | "\\!" { add_char buf '!'; unescape_lexer buf lexbuf }
 | "\\b" { add_char buf '\b'; unescape_lexer buf lexbuf }
 | "\\n" { add_char buf '\n'; unescape_lexer buf lexbuf }
 | "\\r" { add_char buf '\r'; unescape_lexer buf lexbuf }
 | "\\t" { add_char buf '\t'; unescape_lexer buf lexbuf }
 | "\\ " { add_char buf ' '; unescape_lexer buf lexbuf }
 | "\\`" { add_char buf '`'; unescape_lexer buf lexbuf }
 | "\\'" { add_char buf '\''; unescape_lexer buf lexbuf }
 | "\\\"" { add_char buf '"'; unescape_lexer buf lexbuf }
 | _ as c { add_char buf c; unescape_lexer buf lexbuf }
 | eof { () }

{

let escape_string s =
  let len = String.length s in
  let b = Buffer.create (len*2) in
  let add = Buffer.add_string b in
  let iter = function
    | '\\' -> add "\\\\"
    | '$' -> add "\\$"
    | '!' -> add "\\!"
    | '\b' -> add "\\b"
    | '\n' -> add "\\n"
    | '\r' -> add "\\r"
    | '\t' -> add "\\t"
    | ' ' -> add "\\ "
    | '`' -> add "\\`"
    | '\'' -> add "\\'"
    | '"' -> add "\\\""
    | c -> Buffer.add_char b c
  in
  String.iter iter s;
  Buffer.contents b

let unescape_string ?(buffer=Buffer.create 64) s =
  let lexbuf = Lexing.from_string s in
  unescape_lexer buffer lexbuf;
  Buffer.contents buffer
  
}
