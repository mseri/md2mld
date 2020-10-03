(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

let readall filename =
  let ic = open_in filename in
  try
    let n = in_channel_length ic in
    let b = Bytes.create n in
    really_input ic b 0 n;
    close_in ic;
    Bytes.to_string b
  with
  | exn ->
    close_in ic;
    raise exn


(*
let writeall filename content =
  let oc = open_out filename in
  try
    Printf.fprintf oc "%s\n" content ;
    close_out oc
  with exn -> close_out oc ; raise exn
*)

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
  | [] | _ :: _ :: _ ->
    Arg.usage speclist usage_msg;
    exit 1
  | [ filename ] when not (Sys.file_exists filename) ->
    Printf.eprintf "error: file %s not found\n%!" filename;
    exit 2
  | [ filename ] ->
    let min_header = !min_header in
    readall filename
    |> Omd.of_string
    |> Backend.mld_of_md ~min_header
    |> String.trim
    |> print_endline
