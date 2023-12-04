open Format
open Print
open Forth

(* exception StackUnderflow of word *)

type error =
  | StackUnderflow of word
  | NoSubroutine
  | NestedSubroutine
  | InvalidName
  | UndefinedWord

let pp_error p = function
  | StackUnderflow w ->
    fprintf p "Stack underflow on word %a." pp_word w
  | NoSubroutine ->
    fprintf p "Not in subroutine."
  | NestedSubroutine ->
    fprintf p "Nested subroutines is not allowed."
  | InvalidName ->
    fprintf p "Invalid name for subroutine."
  | UndefinedWord ->
    fprintf p "Undefined word."

let error e =
  Format.eprintf "%a@." pp_error e;
  exit 1