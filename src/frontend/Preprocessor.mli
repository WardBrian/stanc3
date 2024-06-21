open Includes_intf

(** Preprocessor for handling include directives *)

val location_of_position : Lexing.position -> Middle.Location.t

val location_span_of_positions :
  Lexing.position * Lexing.position -> Middle.Location_span.t

val current_buffer : unit -> Lexing.lexbuf
(** Buffer at the top of the include stack *)

val current_location_t : unit -> Middle.Location.t
(** Location of the current buffer *)

val size : unit -> int
(** Size of the include stack *)

val init : Lexing.lexbuf -> string -> unit
(** Push a buffer on to the stack to start *)

val update_start_positions : Lexing.position -> unit
(** Update the lex_start_p the lexing buffers on the stack.
  This solves an issue where a parser which started with one lexbuf
  but is finishing with another can have the wrong information
*)

val pop_buffer : unit -> Lexing.lexbuf
(** Pop the buffer at the top of the include stack *)

val add_comment : Ast.comment_type -> unit
val get_comments : unit -> Ast.comment_type list

val included_files : string list ref
(** List of files that have been included *)

val restore_prior_lexbuf : unit -> Lexing.lexbuf
(** Restore to a previous lexing buffer (assumes that one exists) and
  updates positions accordingly. *)

(** The part of the preprocessor that loads #include-d files is
  made generic here to support cases where we have filesystem access
  and cases where we do not.

  See [In_memory_includes] and [Filesystem_includes] for
  implementations of the [LEXBUF_LOCATOR] module.
  *)
module Make (Locator : LEXBUF_LOCATOR) : PREPROCESSOR_LOADER
