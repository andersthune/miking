open Lsp
module Table = Stdune.Table

type t = (Uri.t, Document.t) Table.t

let make () = Table.create (module Uri) 50

let put store uri doc = Table.set store uri doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc
  | None ->
    Error
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:(Format.asprintf "no document found with uri: %a" Uri.pp uri)
         ())

let remove_document store uri =
  match Table.find store uri with
  | None -> ()
  | Some _ -> Table.remove store uri

let get_size store = Table.length store
