{ 
    open Forth
}

let letter = ['a'-'z' 'A'-'Z' '_' '-' '?' '^' '0'-'9']
let word = letter*

rule token = parse
| word as w  { word_of_string w }
| '\n'       { Lexing.newline lexbuf; token lexbuf }
| [' ' '\t'] { token lexbuf }
| '('        { comment lexbuf }
| eof        { }
| _          { failwith "Lexical error." }
and comment = parse
| '\n' { Lexing.newline lexbuf; comment lexbuf }
| eof  { failwith "End of line in commnent." }
| ')'  { token lexbuf }
| _    { comment lexbuf }

{ }