open Std
open Frontend
open Conversion
open Js_of_ocaml

let invoke_driver model_name model flags =
  let warnings = ref [] in
  let compilation_result =
    Return.with_return @@ fun return ->
    let output_callback : Driver.Entry.other_output -> unit = function
      | Warnings w -> warnings := !warnings @ w
      | Info i -> return (Ok (Yojson.Basic.pretty_to_string i))
      | Formatted s
       |DebugOutput s
       |Memory_patterns s
       |Version s
       |Generated s ->
          (* stanc.js will only ever return one output, so we break out
             prematurely *)
          return (Ok s) in
    Driver.Entry.stan2cpp model_name (`Code model) flags output_callback in
  (compilation_result, !warnings)

(** Handle conversion of JS <-> OCaml values invoke driver *)
let stan2cpp_wrapped name code flags includes : stancReturn Js.t =
  let includes, include_reader_warnings = get_includes_lenient includes in
  let compilation_result =
    let open Result.Syntax in
    let* {name; code; driver_flags; color_output} =
      process_flags name code flags includes in
    let+ result, warnings =
      Common.ICE.with_exn_message (fun () ->
          invoke_driver name code driver_flags) in
    (result, warnings, driver_flags.filename_in_msg, code, color_output) in
  match compilation_result with
  | Ok (result, warnings, printed_filename, code, color_output) ->
      let warnings =
        include_reader_warnings
        @ List.map ~f:(Warnings.to_grace ?printed_filename ~code) warnings in
      wrap_result ?printed_filename ~color_output ~code result ~warnings
  | Error non_compilation_error (* either an ICE or malformed JS input *) ->
      wrap_error ~color_output:false ~warnings:include_reader_warnings
        non_compilation_error

(** Like [stan2cpp_wrapped] but just checks the model for correctness, doesn't
    return the generated C++. The output is in a rich object form. Throws errors
    on bad input or ICEs *)
let check_model name code flags includes =
  let includes = get_includes includes in
  let {name; code; driver_flags; color_output= _} =
    process_flags name code flags includes |> res_or_throw in
  let driver_flags =
    { driver_flags with
      warn_uninitialized= true
    ; (* Disable certain flags that we know won't have an affect here *)
      auto_format= false
    ; info= false
    ; version= false
    ; debug_settings= Driver.Flags.default.debug_settings } in
  let warnings = ref [] in
  let output : Driver.Entry.other_output -> unit = function
    | Warnings w -> warnings := !warnings @ w
    | _ -> () in
  let result =
    Common.ICE.with_exn_message (fun () ->
        Driver.Entry.stan2cpp name (`Code code) driver_flags output)
    |> res_or_throw in
  let printed_filename = driver_flags.filename_in_msg in
  let warnings =
    List.map ~f:(Warnings.to_grace ?printed_filename ~code) !warnings in
  let errors =
    match result with
    | Ok _ -> []
    | Error e -> [Errors.to_grace ?printed_filename ~code e] in
  object%js
    val errors = json_of_diagnostics errors
    val warnings = json_of_diagnostics warnings
  end

(** Like [stan2cpp_wrapped] but always formats the model, doesn't return the
    generated C++. The output is in a rich object form. Throws errors on bad
    input or ICEs *)
let format_model name code flags includes =
  let includes = get_includes includes in
  let {name; code; driver_flags; color_output= _} =
    process_flags name code flags includes |> res_or_throw in
  let driver_flags =
    { driver_flags with
      auto_format= true
    ; allow_undefined= true
    ; debug_settings= Driver.Flags.default.debug_settings } in
  let warnings = ref [] in
  let run () =
    let res =
      Return.with_return @@ fun return ->
      let output : Driver.Entry.other_output -> unit = function
        | Warnings w -> warnings := !warnings @ w
        | Formatted s -> return (Ok s)
        | _ -> () in
      Driver.Entry.stan2cpp name (`Code code) driver_flags output in
    res in
  let result = Common.ICE.with_exn_message run |> res_or_throw in
  let printed_filename = driver_flags.filename_in_msg in
  let warnings =
    List.map ~f:(Warnings.to_grace ?printed_filename ~code) !warnings in
  let result, errors =
    match result with
    | Ok formatted -> (Js.def (Js.string formatted), [])
    | Error e -> (Js.undefined, [Errors.to_grace ?printed_filename ~code e])
  in
  object%js
    val result = result [@@optdef]
    val errors = json_of_diagnostics errors
    val warnings = json_of_diagnostics warnings
  end

let model_info name code flags includes =
  let includes = get_includes includes in
  let {name; code; driver_flags; color_output= _} =
    process_flags name code flags includes |> res_or_throw in
  let driver_flags =
    { driver_flags with
      info= true
    ; allow_undefined= true
    ; debug_settings= Driver.Flags.default.debug_settings } in
  let warnings = ref [] in
  let run () =
    let res =
      Return.with_return @@ fun return ->
      let output : Driver.Entry.other_output -> unit = function
        | Warnings w -> warnings := !warnings @ w
        | Info i -> return (Ok i)
        | _ -> () in
      let _ = Driver.Entry.stan2cpp name (`Code code) driver_flags output in
      (* impossible, but needed for typing *)
      return (Ok `Null) in
    res in
  let result = Common.ICE.with_exn_message run |> res_or_throw in
  let printed_filename = driver_flags.filename_in_msg in
  let warnings =
    List.map ~f:(Warnings.to_grace ?printed_filename ~code) !warnings in
  let info, errors =
    match result with
    | Ok info -> (Js.def (js_of_yojson info), [])
    | Error e -> (Js.undefined, [Errors.to_grace ?printed_filename ~code e])
  in
  object%js
    val info = info [@@optdef]
    val errors = json_of_diagnostics errors
    val warnings = json_of_diagnostics warnings
  end

let version () = Js.string Driver.Entry.version

let dump_stan_math_signatures () =
  Js.string @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_sigs ()

let dump_stan_math_distributions () =
  Js.string
  @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_distributions ()

(** Rather than reflect the true JSONRpc type, we just say a message is any Js
    object *)
type message = Js.Unsafe.any

type disposable = < dispose: unit -> Js.Unsafe.any Js.meth > Js.t

(** https://github.com/microsoft/vscode-languageserver-node/blob/main/jsonrpc/src/common/messageReader.ts
*)
type reader = < listen: (message -> unit) -> disposable Js.meth > Js.t

(** https://github.com/microsoft/vscode-languageserver-node/blob/main/jsonrpc/src/common/messageWriter.ts
*)
type writer = < write: message -> unit Promise.t Js.meth > Js.t

module MessageQueue : sig
  type 'a t

  val create : unit -> 'a t
  val put : 'a t -> 'a -> unit
  val take : 'a t -> 'a Lwt.t
end = struct
  open Lwt.Syntax

  type 'a t = 'a Queue.t * unit Lwt_condition.t

  let create () = (Queue.create (), Lwt_condition.create ())

  let put (q, f) v =
    Queue.add v q;
    Lwt_condition.broadcast f ()

  let rec take (q, f) =
    if Queue.is_empty q then
      let* () = Lwt_condition.wait f in
      take (q, f)
    else Lwt.return (Queue.pop q)
end

module IO_js : sig
  include Linol.IO with type 'a t = 'a Lwt.t

  val make_reader : reader -> in_channel
end = struct
  type 'a t = 'a Lwt.t

  include Lwt.Syntax

  let return = Lwt.return
  let failwith = Lwt.fail_with

  let catch f g =
    let bt = Stdlib.Printexc.get_callstack 10 in
    Lwt.catch f (fun exn -> g exn bt)

  let fail e _bt = Lwt.fail e

  type out_channel = writer
  type in_channel = < read: unit -> message Lwt.t >

  let make_reader (reader : reader) : in_channel =
    let adaptor =
      object
        val messages = MessageQueue.create ()
        method on_listen (message : message) = MessageQueue.put messages message
        method read () = MessageQueue.take messages
      end in
    let d = reader##listen adaptor#on_listen in
    Stdlib.Gc.finalise (fun _ -> ignore (d##dispose ())) adaptor;
    (adaptor :> in_channel)

  let send_msg oc ~json =
    Js_of_ocaml_lwt.Promise.to_lwt
      (oc##write (Conversion.js_of_yojson (Yojson.Safe.to_basic json)))

  let read_msg ic =
    let* message = ic#read () in
    (* TODO: smarter conversion back? *)
    catch
      (fun () ->
        let json = Js.to_string (Js._JSON##stringify message) in
        let yojson = Yojson.Safe.from_string json in
        return (Ok yojson))
      (fun e bt -> return (Error (e, bt)))
end

module Jsonrpc2 = Linol.Jsonrpc2.Make (IO_js)
module LSP = Lsp.Server.Make (Jsonrpc2.IO)

let run_js reader writer =
  let s = new LSP.lsp_server in
  let server = Jsonrpc2.create ~ic:(IO_js.make_reader reader) ~oc:writer s in
  let task =
    let shutdown () = s#get_status = `ReceivedExit in
    Jsonrpc2.run ~shutdown server in
  Lwt.ignore_result task

let () =
  (* the stanc function is roughly equivalent to the full CLI *)
  Js.export "stanc" (Js.Unsafe.callback stan2cpp_wrapped);
  Js.export "dump_stan_math_signatures"
    (Js.wrap_callback dump_stan_math_signatures);
  Js.export "dump_stan_math_distributions"
    (Js.wrap_callback dump_stan_math_distributions);
  (* these functions are all cut-down versions of stanc above, with different
     default behavior and easier-to-use return types (e.g., errors are
     represented as an object, rather than text) *)
  Js.export "check_model" (Js.Unsafe.callback check_model);
  Js.export "format_model" (Js.Unsafe.callback format_model);
  Js.export "model_info" (Js.Unsafe.callback model_info);
  Js.export "version" (Js.wrap_callback version);
  Js.export "lsp" (Js.wrap_callback run_js)
