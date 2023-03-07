(** stanup - port stan code from 2.31 or earlier to 2.32+ syntax *)

open Core_kernel
open Core_kernel.Poly
open Frontend

let version = "%%NAME%%3 %%VERSION%%"
let name = "%%NAME%%"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... <model_file.stan[functions]>"

let model_file = ref ""
let pretty_print_program = ref true
let pretty_print_line_length = ref 78
let filename_for_msg = ref ""
let canonicalize_settings = ref {Canonicalize.none with deprecations= true}
let output_file = ref ""
let bare_functions = ref false

let parse_canonical_options (settings : Canonicalize.canonicalizer_settings)
    string =
  match String.lowercase string with
  | "deprecations" -> {settings with deprecations= true}
  | "parentheses" -> {settings with parentheses= true}
  | "braces" -> {settings with braces= true}
  | "includes" -> {settings with inline_includes= true}
  | s ->
      raise
      @@ Arg.Bad
           ( "Unrecognized canonicalizer option '" ^ s
           ^ "'. \n\
              Should be one of 'deprecations', 'parentheses', 'braces', \
              'includes'" )

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--canonicalize"
      , Arg.String
          (fun s ->
            let settings =
              List.fold ~f:parse_canonical_options ~init:!canonicalize_settings
                (String.split s ~on:',') in
            canonicalize_settings := settings )
      , " Enable specific canonicalizations in a comma seperated list. Options \
         are 'deprecations', 'parentheses', 'braces', 'includes'." )
    ; ( "--max-line-length"
      , Arg.Set_int pretty_print_line_length
      , " Set the maximum line length for the formatter. Defaults to 78 \
         characters." )
    ; ( "--print-canonical"
      , Arg.Unit (fun () -> canonicalize_settings := Canonicalize.all)
      , " Prints the canonicalized program. Equivalent to --canonicalize [all \
         options]" )
    ; ( "--version"
      , Arg.Unit
          (fun _ ->
            print_endline (version ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Display stanup version number" )
    ; ( "--o"
      , Arg.Set_string output_file
      , " Take the path to an output file for auto-formatting output (default: \
         no file/print to stdout)" )
    ; ( "--allow-undefined"
      , Arg.Clear Typechecker.check_that_all_functions_have_definition
      , " Do not fail if a function is declared but not defined" )
    ; ( "--include-paths"
      , Arg.String
          (fun str ->
            Preprocessor.include_paths := String.split_on_chars ~on:[','] str )
      , " Takes a comma-separated list of directories that may contain a file \
         in an #include directive (default = \"\")" )
    ; ( "--filename-in-msg"
      , Arg.Set_string filename_for_msg
      , " Sets the filename used in errors. Uses actual filename by default." )
    ]

let model_file_err () =
  Arg.usage options ("Please specify a model_file.\n" ^ usage) ;
  exit 127

let add_file filename =
  if !model_file = "" then model_file := filename
  else raise (Arg.Bad "Please specify only one model_file")

let remove_dotstan s =
  if String.is_suffix ~suffix:".stanfunctions" s then String.drop_suffix s 14
  else String.drop_suffix s 5

let get_ast_or_exit ?printed_filename ?(print_warnings = true)
    ?(bare_functions = false) filename =
  let res, warnings =
    if bare_functions then
      Parse.parse_file Parser.Incremental.functions_only filename
    else Parse.parse_file Parser.Incremental.program filename in
  if print_warnings then
    (Warnings.pp_warnings ?printed_filename) Fmt.stderr warnings ;
  match res with
  | Result.Ok ast -> ast
  | Result.Error err ->
      Errors.pp ?printed_filename Fmt.stderr err ;
      exit 1

let type_ast_or_exit ?printed_filename ast =
  match Typechecker.check_program ast with
  | Result.Ok (p, warns) ->
      Warnings.pp_warnings ?printed_filename Fmt.stderr warns ;
      p
  | Result.Error error ->
      Errors.pp_semantic_error ?printed_filename Fmt.stderr error ;
      exit 1

(*
      I am not using Fmt to print to stderr here because there was a pretty awful
      bug where it would unpredictably fail to flush. It would flush when using
      stdout or when trying to print some strings and not others. I tried using
      Fmt.flush and various other hacks to no avail. So now I use Fmt to build a
      string, and Out_channel to write it.
 *)
let pp_stderr formatter formatee =
  Fmt.str "%a" formatter formatee |> Out_channel.(output_string stderr)

let print_or_write data =
  if !output_file <> "" then Out_channel.write_all !output_file ~data
  else print_endline data

let use_file filename =
  let printed_filename =
    match !filename_for_msg with "" -> None | s -> Some s in
  let ast =
    get_ast_or_exit ?printed_filename filename
      ~print_warnings:(not !canonicalize_settings.deprecations)
      ~bare_functions:!bare_functions in
  (* must be before typecheck to fix up deprecated syntax which gets rejected *)
  let ast = Canonicalize.repair_syntax ast !canonicalize_settings in
  let typed_ast = type_ast_or_exit ?printed_filename ast in
  let canonical_ast =
    Canonicalize.canonicalize_program typed_ast !canonicalize_settings in
  print_or_write
    (Pretty_printing.pretty_print_typed_program ~bare_functions:!bare_functions
       ~line_length:!pretty_print_line_length
       ~inline_includes:!canonicalize_settings.inline_includes canonical_ast )

let mangle =
  String.concat_map ~f:(fun c ->
      Char.(
        if is_alphanum c || c = '_' then to_string c
        else match c with '-' -> "_" | _ -> "x" ^ Int.to_string (to_int c)) )

let main () =
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  if !model_file = "" then model_file_err () ;
  (* if we only have functions, always compile as standalone *)
  if String.is_suffix !model_file ~suffix:".stanfunctions" then
    bare_functions := true ;
  (* Just translate a stan program *)
  if !Typechecker.model_name = "" then
    Typechecker.model_name :=
      mangle
        (remove_dotstan List.(hd_exn (rev (String.split !model_file ~on:'/'))))
      ^ "_model"
  else Typechecker.model_name := mangle !Typechecker.model_name ;
  use_file !model_file

let () = main ()
