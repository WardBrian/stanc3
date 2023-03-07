open! Core_kernel

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty file =
  add_warning Middle.Location_span.empty
    ( "Empty file '" ^ file
    ^ "' detected; this is a valid stan model but likely unintended!" )

let deprecated _token _ = ()
let array_syntax ?(unsized = false) _ = ignore unsized

let drop_array_future () =
  match !warnings with
  | ( _
    , "Variable name 'array' will be a reserved word starting in Stan 2.32.0. \
       Please rename it!" )
    :: tl ->
      warnings := tl
  | _ -> ()

let future_keyword kwrd version (pos1, pos2) =
  add_warning
    (Option.value ~default:Middle.Location_span.empty
       (Middle.Location_span.of_positions_opt pos1 pos2) )
    ( "Variable name '" ^ kwrd ^ "' will be a reserved word starting in Stan "
    ^ version ^ ". Please rename it!" )
