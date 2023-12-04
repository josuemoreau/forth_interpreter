open Forth
open Error
open Dynarray

type def = {
  code: word list;
  exec: unit -> unit
}

type defs = (string, def) Hashtbl.t

exception BeginDef

let unop w f ds =
  try
    let n1 = pop ds in
    push ds (f n1)
  with _ -> error (StackUnderflow w)

let binop w f ds =
  try
    let n2 = pop ds in
    let n1 = pop ds in
    push ds (f n1 n2)
  with _ -> error (StackUnderflow w)

(* let backup = ref None *)
let name = ref ""

let exec_word h ds rs w : unit =
  let _ = rs in
  let _ = h in
  match w with
  | Int n -> push ds n
  | Add1 -> unop w ((+) 1) ds
  | Sub1 -> unop w (fun x -> x - 1) ds
  | Add2 -> unop w ((+) 2) ds
  | Add4 -> unop w ((+) 4) ds
  | Times2 -> unop w (fun x -> x lsl 1) ds
  | Div2 -> unop w (fun x -> x lsr 1) ds
  | Add -> binop w (+) ds
  | Sub -> binop w (-) ds
  | Other s -> (match Hashtbl.find h s with
                | k -> k.exec ()
                | exception Not_found ->
                  error UndefinedWord)
  | _ -> failwith "Not implemented yet."

let createK f = { code = []; exec = f } 

let rec exec_list h ds rs k def w : def =
  match w with
  | [] -> k
  | BSubroutine :: lw ->
    if def then error NestedSubroutine
    else (match lw with
         | [] -> raise BeginDef
         | Other s :: lw -> name := s;
                            let k = createK (fun () -> ()) in
                            exec_list h ds rs k true lw
         | _ -> error InvalidName)
  | ESubroutine :: lw ->
    Hashtbl.add h !name k;
    name := "";
    let k = createK (fun () -> ()) in
    exec_list h ds rs k false lw
  | Other s :: lw ->
    let f = try (Hashtbl.find h s).exec with Not_found -> error UndefinedWord in
    if def then
      let k = { code = k.code; exec = fun () -> k.exec (); f () } in
      exec_list h ds rs k true lw
    else (f (); exec_list h ds rs k def lw)
  | w :: lw ->
    if def then
      let k = { code = k.code; exec = fun () -> k.exec (); exec_word h ds rs w } in
      exec_list h ds rs k true lw
    else (exec_word h ds rs w; exec_list h ds rs k def lw)