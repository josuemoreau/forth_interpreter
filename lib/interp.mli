open Forth

type def

type defs = (string, def) Hashtbl.t

exception BeginDef

val createK : (unit -> unit) -> def
val exec_list : defs -> int Dynarray.t -> int Dynarray.t -> def -> bool -> word list -> def