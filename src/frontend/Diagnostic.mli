open Grace

val locate :
     ?printed_filename:string
  -> ?code:string
  -> Middle.Location_span.t
  -> 'a Diagnostic.t
  -> 'a Diagnostic.t

val included_diagnostic :
     ?printed_filename:string
  -> ?code:string
  -> Middle.Location.t
  -> Diagnostic.Label.t list

val range_of_loc_span :
  ?printed_filename:string -> ?code:string -> Middle.Location_span.t -> Range.t

val pp : 'a Diagnostic.t Fmt.t

module Json_printer : sig
  val to_yojson :
    ?code_to_string:('a -> string) -> 'a Diagnostic.t -> Yojson.Basic.t

  val pp_json : 'a Diagnostic.t Fmt.t
end
