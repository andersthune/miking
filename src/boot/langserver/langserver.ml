open Lsp
open Types

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Fiber.return
  @@ Error
       (make_error ~code:InternalError ~message:"Request not supported yet!" ())

let initialize_info : InitializeResult.t =
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Full ~willSave:false
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

let main =
  let _ = initialize_info in
  let _ = not_supported in
  print_endline "Hello, world!"
