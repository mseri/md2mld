(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

let readall ic =
  let return_input ic buf =
    let () = close_in ic in
    buf |> Buffer.to_bytes |> Bytes.to_string
  in
  let channel_size =
    try Some (in_channel_length ic)
    with Sys_error _ -> None
  in
  let buf = Buffer.create @@ Option.value ~default:4096 channel_size in
  try
    let () =
      while true do
        Buffer.add_channel buf ic 4096
      done
    in return_input ic buf
  with
  | End_of_file ->
    return_input ic buf
  | _ as exn ->
    let () = close_in ic in
    raise exn

(*
let writeall filename content =
  let oc = open_out filename in
  try
    Printf.fprintf oc "%s\n" content ;
    close_out oc
  with exn -> close_out oc ; raise exn
*)

let get_input_channel file_arg =
  match file_arg with
  | [] -> stdin
  | [ filename ] -> open_in filename
  | _ -> raise (Invalid_argument "Cannot handle multiple input files")

let _ =
  let input_files = ref [] in
  let min_header = ref 0 in
  let speclist =
    [ ( "-min-header"
      , Arg.Set_int min_header
      , "Minimal section header level. Defaults to 0." )
    ]
  in
  let usage_msg =
    "Usage: md2mld [OPTIONS] FILENAME\n\
     md2mld converts a Markdown-format file into the mld format used by odoc to render \
     HTML documentation or OCaml libraries.\n\n\
     Options:"
  in
  Arg.parse speclist (fun filename -> input_files := filename :: !input_files) usage_msg;
  match !input_files with
  | [ filename ] when not (Sys.file_exists filename) ->
    Printf.eprintf "error: file %s not found\n%!" filename;
    exit 2
  | [] | [ _ ] ->
    let min_head_lvl = !min_header in
    let ic = get_input_channel !input_files in
    readall ic
    |> Omd.of_string
    |> Backend.mld_of_md ~min_head_lvl
    |> String.trim
    |> print_endline
  | _ ->
    Arg.usage speclist usage_msg;
    exit 1

