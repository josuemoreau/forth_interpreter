open Forth

let string_of_word = function
  | Int n -> string_of_int n
  | Neg -> "0<" | Zero -> "0=" | Pos -> "0>"
  | Lt -> "<" | Neq -> "<>" | Eq -> "=" | Gt -> ">" | LtU -> "U<" | GtU -> "U>"
  | Branch0 -> "0BRANCH" | Branch -> "BRANCH"
  | Add1 -> "1+" | Sub1 -> "1-" | Add2 -> "2+" | Add4 -> "4+"
  | Times2 -> "2*" | Div2 -> "2/"
  | Add -> "+" | AddPtr -> "+!" | Sub -> "-" | AddD -> "D+"
  | MulU -> "U*" | DivU -> "U/MOD"
  | NegateD -> "DNEGATE" | Negate -> "NEGATE"
  | Store -> "!" | Load -> "@" | StoreD -> "D!" | LoadD -> "D@"
  | RetPush -> "R" | RetPop -> "R>" | RetCopy -> "R@"
  | DDrop -> "DDROP" | DDup -> "DDUP" | DSwap -> "DSWAP"
  | SignExtD -> "S-D"
  | CDup -> "?DUP" | Dup -> "DUP" | Drop -> "DROP" | Over -> "OVER"
  | Pick -> "PICK" | Rot -> "ROT" | Roll -> "ROLL" | Swap -> "SWAP"
  | Nop  -> "NOP"
  | I -> "I" | I' -> "I'" | J -> "J" | Leave -> "Leave"
  | And -> "AND" | Not -> "NOT" | Or -> "OR" | Xor -> "XOR"
  | Abs -> "ABS"
  | BSubroutine -> ":" | ESubroutine -> ";"
  | Other s -> s
 
let pp_word p w =
  Format.fprintf p "%s" (string_of_word w)

let print_word w =
  Format.printf "%a" pp_word w