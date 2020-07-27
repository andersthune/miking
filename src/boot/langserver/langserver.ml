open Lsp
open Types
open Eval
open Repl

module State = struct
  type t = {
    store : Document_store.t
  }

  let empty () = {
    store = Document_store.make ()
  }
end

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Fiber.return
  @@ Error
       (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let initialize_info : InitializeResult.t =
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Incremental ~willSave:false
         ~willSaveWaitUntil:false ())
  in
  let completionProvider = CompletionOptions.create () in
  let capabilities =
    ServerCapabilities.create ~textDocumentSync ~completionProvider ()
  in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"mcorelsp" ~version:"0.1" ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let on_request :
    type resp.
       State.t Server.t
    -> resp Client_request.t
    -> (resp * State.t, Jsonrpc.Response.Error.t) result Fiber.t =
  fun server req ->
    let state = Server.state server in
    let store = state.State.store in
    match req with
    | Client_request.Initialize _ -> Fiber.return @@ Ok (initialize_info, state)
    | Client_request.Shutdown -> Fiber.return @@ Ok ((),state)
    | Client_request.TextDocumentCompletion
        { textDocument = { uri }; position; context = _ } ->
      let uri = Uri.t_of_yojson (`String uri) in
      let { Position.line = line; Position.character = character } = position in
      let path = Uri.to_path uri in
      let prog_result = Document_store.get store uri in
      let try_to_get_completions prog =
        let prog_text = Document.text prog in
        prerr_endline "Starting completion";
        Format.eprintf "Program text: %s\n" prog_text;
        try
          let ast = parse_prog_or_mexpr path prog_text in
          initialize_envs ();
          parsed_files := [];
          let new_envs, _ = ast
            |> merge_includes (Filename.dirname path) [path]
            |> eval_with_envs ~skip_eval:true !repl_envs in
          repl_envs := new_envs;
          let line_to_complete = BatString.cut_on_char '\n' line prog_text in
          let _,raw_completions = get_completions line_to_complete character in
          prerr_endline "Got completion candidates:";
          List.iter prerr_endline raw_completions;
          let completions = List.map (fun x -> CompletionItem.create ~label:x ()) raw_completions in
          Ok (Some (`List completions), state)
        with e -> prerr_endline (Printexc.to_string e); Ok (None, state)
      in
      Fiber.return @@ Result.bind prog_result try_to_get_completions
    | _ -> not_supported ()

let on_notification server (notification : Client_notification.t) : State.t Fiber.t =
  let state = Server.state server in
  let store = state.State.store in
  match notification with
  | TextDocumentDidOpen params ->
    let tdoc = Document.make params in
    let uri = Document.documentUri tdoc in
    Document_store.put store uri tdoc;
    Fiber.return state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let uri = Uri.t_of_yojson (`String uri) in
    Document_store.remove_document store uri;
    Fiber.return state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges }
    -> (
    let uri = Uri.t_of_yojson (`String uri) in
    match Document_store.get store uri with
    | Error e ->
      Format.eprintf "uri doesn't exist %s@.%!" e.message;
      Fiber.return state
    | Ok prev_doc ->
      let doc = Document.apply_content_change ?version (List.hd contentChanges) prev_doc
      in
      Document_store.put store uri doc;
      Fiber.return state )
  | _ -> Fiber.return state

let main =
  let scheduler = Scheduler.create () in
  let handler =
    let on_request = { Server.Handler.on_request = on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let stream =
    let io = Io.make stdin stdout in
    Rpc.Stream_io.make scheduler io
  in
  let server = Server.make handler stream (State.empty ()) in
  Scheduler.run scheduler (Server.start server)
