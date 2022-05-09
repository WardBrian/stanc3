val read_lines : string -> string list option
(** Return the lines of the file as a list of strings *)

val lexbuf_of : string -> Lexing.lexbuf option
(* Returns a [Lexing.lexbuf] from the file. In the Caml
   interface this is equivalent to [In_channel.create path |> Lexing.from_channel ],
   e.g. no metadata is set.
*)
