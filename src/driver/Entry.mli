(** The main entrypoint for Stan -> C++ compilation *)

val stan2cpp : string -> unit
(** The main function of the compiler. Takes in the model's name,
  the model code, compiler settings, and a callback for all non-C++
  output
  *)
