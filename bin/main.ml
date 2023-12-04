open Forth_interpreter
open Print
open Forth
open Interp

let () = print_endline "Hello, World!"

let () = print_word DDup; Format.printf "@."

let () =
  let h = Hashtbl.create 10 in
  let ds = Dynarray.create 10 0 in
  let rs = Dynarray.create 10 0 in
  let lw = [Int 5; BSubroutine; Other "test"; Sub; ESubroutine; Int 4; Other "test"] in
  let k = createK (fun () -> ()) in
  let _ = exec_list h ds rs k false lw in
  Dynarray.iter (fun n -> Format.printf "%d " n) ds;
  Format.printf "@."