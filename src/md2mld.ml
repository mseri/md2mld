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
    really_input ic b 0 n ; close_in ic ; Bytes.to_string b
  with exn -> close_in ic ; raise exn

(*
let writeall filename content =
  let oc = open_out filename in
  try
    Printf.fprintf oc "%s\n" content ;
    close_out oc
  with exn -> close_out oc ; raise exn
*)

let _ =
  let min_header = ref 0 in
  let no_file_specified = ref true in
  let speclist = [
    ("-min-header", Arg.Set_int min_header,
     "Sets the section level associated to <h1> in the generated odoc file \
      (all the other values will be determined incrementally). Defaults to 0.")
  ] in
  let usage_msg = "md2mld converts a Markdown-format file into the mld format \
                   used by odoc to render HTML documentation or OCaml \
                   libraries. The following options are available:\n"
  in
  let runner filename =
    no_file_specified := false;
    if not (Sys.file_exists filename) then (
      Printf.eprintf "error: file %s not found\n%!" filename ;
      exit 2 ) ;
    let min_header = !min_header in
    readall filename |> Omd.of_string |> Backend.mld_of_md ~min_header |> print_endline
  in
  Arg.parse speclist runner usage_msg;
  if !no_file_specified then (
    Arg.usage speclist usage_msg;
    exit 1 )

