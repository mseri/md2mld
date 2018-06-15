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
  with exn -> (close_in ic; raise exn)

let writeall filename content =
  let oc = open_out filename in
  try
    Printf.fprintf oc "%s\n" content;
    close_out oc;
  with exn -> (close_out oc; raise exn)

let _ = 
  if (Array.length Sys.argv) < 2 then
    (Printf.eprintf "usage: %s filename.md\n" Sys.executable_name; exit 1);
  let filename = Sys.argv.(1) in
  if not (Sys.file_exists filename) then
    (Printf.eprintf "error: file %s not found\n" filename; exit 1);
  (* let outname = 
    String.sub filename 0 (String.length filename -4)
    ^ ".mld"
  in *)
  readall filename
  |> Omd.of_string
  |> Backend.mld_of_md
  |> print_endline
  (*|> writeall outname*)
