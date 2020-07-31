open Jupyter_kernel
open Repl
open Eval
open Lwt

let current_output = ref (BatIO.output_string ())
let other_actions = ref []

let text_data_of_string str =
  Client.Kernel.mime ~ty:"text/plain" str

let kernel_output_string str = BatIO.nwrite !current_output str
let kernel_output_ustring ustr = ustr |> Ustring.to_utf8 |> kernel_output_string

let _ = Py.initialize ~version:3 ()
let ocaml_module = Py.Import.add_module "_mcore_kernel"

(* Set Python's sys.stdout to our own ocaml function to handle Python prints *)
let init_py_print () =
  let py_ocaml_print args =
    kernel_output_string (Py.String.to_string args.(0));
    Py.none
  in
  Py.Module.set_function ocaml_module "ocaml_print" py_ocaml_print;
  ignore @@ Py.Run.eval ~start:Py.File "
import sys
from _mcore_kernel import ocaml_print
class OCamlPrint:
    def write(self, str):
        ocaml_print(str)

sys.stdout = OCamlPrint()"

let init_py_mpl () =
  ignore @@ Py.Run.eval ~start:Py.File "
import os, sys
sys.path.append(os.path.expanduser('~') + '/.local/lib/mcore/kernel')
os.environ['MPLBACKEND']='module://mpl_backend'";
  let py_ocaml_show args =
    let data = Py.String.to_string args.(0) in
    other_actions := Client.Kernel.mime ~base64:true ~ty:"image/png" data :: !other_actions;
    Py.none
  in
  Py.Module.set_function ocaml_module "ocaml_show" py_ocaml_show

let init () =
  initialize_envs ();
  Mexpr.program_output := kernel_output_ustring;
  Py.Module.set_function ocaml_module "after_exec" (fun _ -> Py.none);
  init_py_print ();
  init_py_mpl ();
  return ()

let is_expr pycode =
  try
    ignore @@ Py.compile ~source:pycode ~filename:"" `Eval;
    true
  with _ -> false

let eval_python code =
  let statements, expr =
    try BatString.rsplit code ~by:"\n"
    with Not_found -> ("", code)
  in
  let py_val = if is_expr expr then
    (ignore @@ Py.Run.eval ~start:Py.File statements;
    Py.Run.eval expr)
  else
    Py.Run.eval ~start:Py.File code
  in
  if py_val = Py.none then
    None
  else
    Some(Py.Object.to_string py_val)

let visualize_model code count =
  let open Ast in
  let seq2ustring t =
    match t with
    | TmSeq(fi, seq) -> tmseq2ustring fi seq
    | _ -> failwith "Not a string yo"
  in
  let model_str = code
    |> parse_prog_or_mexpr (Printf.sprintf "In [%d]" count)
    |> repl_eval_ast
    |> seq2ustring
    |> Ustring.to_utf8
  in
  let iframe_str = {|<embed src="http://localhost:3000/" width="100%" height="400"</embed>|} in
  let file = "/home/andersthune/Documents/work/kth/summer20/miking-ipm/src/visual/webpage/js/data-source.js" in
  let oc = open_out file in
  Printf.fprintf oc "%s\n" model_str;
  close_out oc;
  other_actions := Client.Kernel.mime ~ty:"text/html" iframe_str::!other_actions;
  None

let exec ~count code =
  let magic_indicator, content =
    try BatString.split code ~by:"\n"
    with Not_found -> ("", code)
  in
  try
    let result =
      match magic_indicator with
      | "%%python" -> eval_python content
      | "%%visualize" -> visualize_model content count
      | _ ->
        parse_prog_or_mexpr (Printf.sprintf "In [%d]" count) code
        |> repl_eval_ast
        |> repl_format
        |> Option.map (Ustring.to_utf8)
    in
    ignore @@ Py.Module.get_function ocaml_module "after_exec" [||];
    let new_actions =
      match BatIO.close_out !current_output with
      | "" -> !other_actions
      | s -> text_data_of_string s :: !other_actions
    in
    let actions = List.rev new_actions in
    current_output := BatIO.output_string ();
    other_actions := [];
    return (Ok { Client.Kernel.msg=result
               ; Client.Kernel.actions=actions})
  with e -> return (Error (error_to_ustring e |> Ustring.to_utf8))

let complete ~pos str =
  let start_pos, completions = get_completions str pos in
  return { Client.Kernel.completion_matches = completions
         ; Client.Kernel.completion_start = start_pos
         ; Client.Kernel.completion_end = pos}

let main =
  let mcore_kernel =
    Client.Kernel.make
      ~language:"MCore"
      ~language_version:[0; 1]
      ~file_extension:".mc"
      ~codemirror_mode:"mcore"
      ~banner:"The core language of Miking - a meta language system
for creating embedded domain-specific and general-purpose languages"
      ~init:init
      ~exec:exec
      ~complete:complete
      ()
  in
  let config = Client_main.mk_config ~usage:"Usage: mcore_kernel --connection-file {connection_file}" () in
  let kernel = Client_main.main ~config:config ~kernel:mcore_kernel in
  let ipm_server = Lwt_process.exec
    ~cwd:"/home/andersthune/Documents/work/kth/summer20/miking-ipm"
    ("", [|"node"; "src/visual/boot.js"; "test/test.mc"|])
  in
  Lwt_main.run (kernel >>= fun _ -> ipm_server)
