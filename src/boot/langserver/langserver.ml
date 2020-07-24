open Lsp
open Types
open Eval
open Repl

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Fiber.return
  @@ Error
       (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let initialize_info : InitializeResult.t =
  let completionProvider = CompletionOptions.create () in
  let capabilities =
    ServerCapabilities.create ~completionProvider ()
  in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"mcorelsp" ~version:"0.1" ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let on_request :
    type resp.
       unit Server.t
    -> resp Client_request.t
    -> (resp * unit, Jsonrpc.Response.Error.t) result Fiber.t =
  fun _ req ->
    match req with
    | Client_request.Initialize _ -> Fiber.return @@ Ok (initialize_info, ())
    | Client_request.Shutdown -> Fiber.return @@ Ok ((),())
    | Client_request.TextDocumentCompletion
        { textDocument = { uri }; position; context = _ } ->
      let uri = Uri.t_of_yojson (`String uri) in
      let { Position.line = line; Position.character = character } = position in
      let path = Uri.to_path uri in
      let lines = BatFile.lines_of path in
      begin
        try
          let prog_text = BatEnum.reduce (Printf.sprintf "%s\n%s") lines in
          let ast = parse_prog_or_mexpr path prog_text in
          initialize_envs ();
          parsed_files := [];
          let new_envs, _ = ast
            |> merge_includes (Filename.dirname path) [path]
            |> eval_with_envs ~skip_eval:true !repl_envs in
          repl_envs := new_envs;
          let line_to_complete = lines
            |> BatEnum.skip (line - 1)
            |> BatEnum.get_exn in
          let _,raw_completions = get_completions line_to_complete character in
          let completions = List.map (fun x -> CompletionItem.create ~label:x ()) raw_completions in
          Fiber.return @@ Ok (Some (`List completions), ())
        with _ -> Fiber.return @@ Ok (None, ())
      end
    | _ -> not_supported ()

let on_notification server (_ : Client_notification.t) : unit Fiber.t =
  Fiber.return @@ Server.state server

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
  let server = Server.make handler stream () in
  Scheduler.run scheduler (Server.start server)
