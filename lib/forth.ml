type word =
  | Int of int
  | Neg | Zero | Pos | Lt | Neq | Eq | Gt | LtU | GtU
  | Branch0 | Branch
  | Add1 | Sub1 | Add2 | Times2 | Div2 | Add4
  | Add | AddPtr | Sub | AddD | NegateD | Negate | MulU | DivU
  | Store | Load | StoreD | LoadD
  | RetPush | RetPop | RetCopy
  | DDrop | DDup | DSwap | SignExtD
  | BSubroutine | ESubroutine
  | CDup | Dup | Drop | Over | Pick | Roll | Rot | Swap
  | Nop
  | I | I' | J | Leave
  (* | Lit *)
  | And | Not | Or | Xor
  | Abs
  | Other of string

let word_of_string = function
  | "0<" -> Neg | "0=" -> Zero | "0>" -> Pos
  | "<" -> Lt | "<>" -> Neq | "=" -> Eq | ">" -> Gt | "U<" -> LtU | "U>" -> GtU
  | "0BRANCH" -> Branch0 | "BRANCH" -> Branch
  | "1+" -> Add1 | "1-" -> Sub1 | "2+" -> Add2 | "4+" -> Add4
  | "2*" -> Times2 | "2/" -> Div2
  | "+" -> Add | "+!" -> AddPtr | "-" -> Sub | "D+" -> AddD
  | "U*" -> MulU | "U/MOD" -> DivU
  | "DNEGATE" -> NegateD | "NEGATE" -> Negate
  | "!" -> Store | "@" -> Load | "D!" -> StoreD | "D@" -> LoadD
  | "R" -> RetPush | "R>" -> RetPop | "R@" -> RetCopy
  | "DDROP" -> DDrop | "DDUP" -> DDup | "DSWAP" -> DSwap
  | "S-D" -> SignExtD
  | "?DUP" -> CDup | "DUP" -> Dup | "DROP" -> Drop | "OVER" -> Over
  | "PICK" -> Pick | "ROT" -> Rot | "ROLL" -> Roll | "SWAP" -> Swap
  | "NOP" -> Nop
  | "I" -> I | "I'" -> I' | "J" -> J | "LEAVE" -> Leave
  | "AND" -> And | "NOT" -> Not | "OR" -> Or | "XOR" -> Xor
  | "ABS" -> Abs
  | ":" -> BSubroutine | ";" -> ESubroutine
  | s ->
    try Int (int_of_string s)
    with Failure _ -> Other s

let word_of_string_no_case s =
  s |> String.capitalize_ascii |> word_of_string