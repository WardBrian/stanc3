(* inspired by https://github.com/panglesd/slipshow/tree/main/src/lspishow *)

let grace_to_lsp
    ({labels; severity; message; notes; code= _} : 'a Grace.Diagnostic.t) =
  let open Linol.Lsp.Types in
  (* TODO handle more than one location, etc *)
  let rect_of_byte_index ~in_ (idx : Grace.Byte_index.t) =
    let open Grace_source_reader.Line in
    let line = of_byte_index in_ idx in
    let col = column_offset ~in_ line idx in
    let line = (line.idx :> int) in
    Position.{line; character= col - 1} in
  let range_of_grange range =
    let sd = Grace_source_reader.open_source (Grace.Range.source range) in
    Range.
      { start= rect_of_byte_index ~in_:sd (Grace.Range.start range)
      ; end_= rect_of_byte_index ~in_:sd (Grace.Range.stop range) } in
  Grace_source_reader.with_reader @@ fun () ->
  let primary, secondary =
    labels
    |> List.partition (fun Grace.Diagnostic.Label.{priority; _} ->
        Grace.Diagnostic.Priority.is_primary priority) in
  let primary = List.hd primary in
  let severity =
    match severity with
    | Grace.Diagnostic.Severity.Bug | Error -> DiagnosticSeverity.Error
    | Warning -> Warning
    | Help -> Hint
    | Note -> Information in
  let main =
    let header = Grace.Diagnostic.Message.to_string message in
    let message = Grace.Diagnostic.Message.to_string primary.message in
    let message =
      if not (String.equal header message) then
        Format.sprintf "%s\n%s" header message
      else message in
    let message =
      if List.is_empty notes then message
      else Fmt.str "%s%a" message (Fmt.list Grace.Diagnostic.Message.pp) notes
    in
    Diagnostic.create ~message:(`String message) ~severity
      ~range:(range_of_grange primary.range)
      () in
  main
  :: List.map
       (fun Grace.Diagnostic.Label.{message; range; _} ->
         Diagnostic.create
           ~message:(`String (Grace.Diagnostic.Message.to_string message))
           ~severity:DiagnosticSeverity.Information
             (* TODO make these DiagnosticRelatedInformation? *)
           ~range:(range_of_grange range) ())
       secondary

let error_to_diagostic ~code e =
  let diag = Frontend.Errors.to_grace ~code e in
  grace_to_lsp diag

let warnings_to_diagnostics ~code ws =
  ws
  |> List.map (Frontend.Warnings.to_grace ~code)
  |> List.concat_map grace_to_lsp

let check code =
  let res, warns = Frontend.Parse.parse_program (`Code code) in
  warnings_to_diagnostics ~code warns
  @
  match res with
  | Error e -> error_to_diagostic ~code e
  | Ok ast -> (
      let res = Frontend.Typechecker.check_program ast in
      match res with
      | Ok (_ast, warns) -> warnings_to_diagnostics ~code warns
      | Error e -> error_to_diagostic ~code (Frontend.Errors.Semantic_error e))

let spawn f =
  Lwt.async (fun () ->
      Lwt.catch f (fun exn ->
          Printf.eprintf "uncaught exception in `spawn`:\n%s\n%!"
            (Printexc.to_string exn);
          Lwt.return ()))

module Make (IO : Linol.BaseIO with type 'a t = 'a Lwt.t) = struct
  module Server = Linol.Server.Make (IO)

  class lsp_server =
    object (self)
      inherit Server.server as super
      method spawn_query_handler f = spawn f

      method! on_req_initialize ~notify_back
          (params : Linol.Lsp.Types.InitializeParams.t) =
        let _wsf = params.workspaceFolders in
        let _uri = params.rootUri in
        let _pth = params.rootPath in
        let root =
          match params.workspaceFolders with
          | Some ws ->
              Option.map
                (List.map (fun (x : Linol.Lsp.Types.WorkspaceFolder.t) -> x.uri))
                ws
          | None -> (
              match params.rootUri with
              | Some root -> Some [root]
              | None -> None) in
        let _roots = Option.value root ~default:[] in
        super#on_req_initialize ~notify_back params

      method! config_completion =
        Some
          (Linol.Lsp.Types.CompletionOptions.create ~triggerCharacters:["~"] ())

      (* method! config_hover = Some (`Bool true) *)
      (* method! config_definition = Some (`Bool true) *)

      (* method! config_modify_capabilities capabilities = *)
      (*   let capabilities = super#config_modify_capabilities capabilities in *)
      (*   { capabilities with *)
      (*   ; referencesProvider= Some (`Bool true) *)
      (*   ; definitionProvider= Some (`Bool true) *)
      (*   ; documentSymbolProvider= Some (`Bool true) } *)

      method! on_req_completion ~notify_back:_ ~id:_ ~uri:_ ~pos:_ ~ctx:_
          ~workDoneToken:_ ~partialResultToken:_ _doc_state =
        let res =
          let completions =
            List.map
              (fun id -> Linol.Lsp.Types.CompletionItem.create ~label:id ())
              ["foo"; "bar"; "baz"; "\"hello world!\""] in
          Some (`List completions) in
        Lwt.return res

      method on_notif_doc_did_open ~notify_back _d ~content : unit Lwt.t =
        (* TODO #includes support? *)
        notify_back#send_diagnostic (check content)

      method on_notif_doc_did_change ~notify_back _d _c ~old_content:_old
          ~new_content =
        notify_back#send_diagnostic (check new_content)

      method! on_notif_doc_did_save ~notify_back params =
        match self#find_doc params.textDocument.uri with
        | Some d -> notify_back#send_diagnostic (check d.content)
        | None -> Lwt.return ()

      method on_notif_doc_did_close ~notify_back d : unit Lwt.t =
        match self#find_doc d.uri with
        | Some d -> notify_back#send_diagnostic (check d.content)
        | None -> Lwt.return ()
    end
end
