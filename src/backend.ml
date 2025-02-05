(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(* From https://github.com/ocaml/omd/pull/215 *)

type t =
  | TripleSurround of string * t * string * t * string
  | Surround of string * t * string
  | BlockSurround of string * t * string
  | GeneralBlock of t
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

let nl = Raw "\n"

let escape_uri s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ( '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '=' | '+' | '$' | ','
        | '/' | '?' | '%' | '#'
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '-' | '_' | '.' | '~' ) as c ->
          Buffer.add_char b c
      | '&' -> Buffer.add_string b "&amp;"
      | _ as c -> Printf.bprintf b "%%%2X" (Char.code c))
    s;
  Buffer.contents b


let escape_mld_special_characters s =
  let add_char buf c =
    match c with
    | '{' | '}' | '[' | ']' | '@' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    | _ -> Buffer.add_char buf c
  in
  (* Allocate a buffer for the worst possible case when every character needs to be escaped.
     Since Markdown paragraphs are rarely longer than a few kilobytes,
     that's arguably better than reallocations.
  *)
  let buf = Buffer.create @@ (String.length s * 2) in
  let () = String.iter (fun c -> add_char buf c) s in
  buf |> Buffer.to_bytes |> Bytes.to_string


let to_plain_text t =
  let buf = Buffer.create 1024 in
  let rec go = function
    | Text t -> Buffer.add_string buf t
    | Concat (t1, t2) ->
        go t1;
        go t2
    | _ -> ()
  in
  go t;
  Buffer.contents buf


let rec add_to_buffer buf = function
  | TripleSurround (s1, t1, s2, t2, s3) ->
      Printf.bprintf buf "%s%a%s%a%s" s1 add_to_buffer t1 s2 add_to_buffer t2 s3
  | Surround (s1, t1, s2) -> Printf.bprintf buf "%s%a%s" s1 add_to_buffer t1 s2
  | BlockSurround (s1, t1, s2) ->
      Printf.bprintf buf "\n%s%a%s\n" s1 add_to_buffer t1 s2
  | GeneralBlock t -> Printf.bprintf buf "\n%a\n" add_to_buffer t
  | Text s -> Buffer.add_string buf s
  | Raw s -> Buffer.add_string buf s
  | Null -> ()
  | Concat (t1, t2) ->
      add_to_buffer buf t1;
      add_to_buffer buf t2


let text s = Text s
let raw s = Raw s

let concat s1 s2 =
  match (s1, s2) with Null, s | s, Null -> s | s1, s2 -> Concat (s1, s2)


let concat_map f l = List.fold_left (fun accu x -> concat accu (f x)) Null l

let cross_reference_words =
  [
    "module";
    "modtype";
    "class";
    "classtype";
    "val";
    "type";
    "exception";
    "attribute";
    "method";
    "section";
    "const";
    "recfield";
  ]


let is_cross_reference_regexps =
  cross_reference_words |> List.map (fun w -> Str.regexp_string (w ^ ":"))


let inferred_cross_reference = Str.regexp_string "ref:"

let rec inline (inl : 'attr Omd.inline) =
  match inl with
  | Concat (_attr, l) -> concat_map inline l
  | Text (_attr, s) -> s |> escape_mld_special_characters |> text
  | Emph (_attr, il) -> Surround ("{e ", inline il, "}")
  | Strong (_attr, il) -> Surround ("{b ", inline il, "}")
  | Code (_attr, s) -> Surround ("[", text s, "]")
  | Hard_break _attr -> text "\n\n"
  | Soft_break _attr -> text "\n"
  | Html (_attr, body) -> Surround ("{%html: ", text body, "%}")
  | Link (_attr, { label; destination; title = _ }) -> (
      let cross_reference =
        match label with
        | Text (_attr, s) when s == destination ->
            if Str.string_match inferred_cross_reference destination 0 then
              Some (Str.string_after destination 4)
            else if
              List.exists
                (fun r -> Str.string_match r destination 0)
                is_cross_reference_regexps
            then Some destination
            else None
        | _ -> None
      in
      match cross_reference with
      | Some cross_reference -> Surround ("{!", text cross_reference, "}")
      | None ->
          TripleSurround ("{{: ", text destination, "} ", inline label, "}"))
  | Image (_attr, { label; destination; title }) ->
      let img =
        "<img src=\"" ^ escape_uri destination ^ "\" alt=\""
        ^ to_plain_text (inline label)
        ^ "\""
        ^ (match title with
          | None -> ""
          | Some title -> " title=\"" ^ title ^ "\"")
        ^ "/>"
      in
      Surround ("{%html: ", text img, "%}")


type ctx = { min_head_lvl : int; in_html : bool }

let rec block ctx (bl : 'attr Omd.block) =
  match bl with
  | Blockquote (_attr, q) ->
      let html =
        BlockSurround
          ( "<blockquote>",
            concat_map (block { ctx with in_html = true }) q,
            "</blockquote>" )
      in
      if ctx.in_html then html else Surround ("{%html: ", html, "%}")
  | Paragraph (_attr, md) -> GeneralBlock (inline md)
  | List (_attr, ty, sp, bl) ->
      let sign = match ty with Ordered _ -> "{+ " | Bullet _ -> "{- " in
      let li t =
        let block' (t : 'attr Omd.block) =
          match (t, sp) with
          | Paragraph (_a, t), Tight -> concat (inline t) nl
          | _ -> block ctx t
        in
        let nl = if sp = Tight then Null else nl in
        Surround (sign, concat nl (concat_map block' t), "}")
      in
      let list_type =
        match ty with Ordered _ -> "{ol " | Bullet _ -> "{ul "
      in
      concat (Surround (list_type, concat_map li bl, "}")) nl
  | Code_block (_attr, label, code) ->
      let opening = if label <> "" then "{@" ^ label ^ "[\n" else "{[\n" in
      BlockSurround (opening, text code, "]}")
  | Thematic_break _attr -> GeneralBlock (text "***")
  | Html_block (_attr, body) -> raw body
  | Heading (_attr, level, text) ->
      BlockSurround
        ( "{"
          ^ (match level + ctx.min_head_lvl with
            | 1 -> "0"
            | 2 -> "1"
            | 3 -> "2"
            | 4 -> "3"
            | 5 -> "4"
            | _ -> "5")
          ^ " ",
          inline text,
          "}" )
  | Definition_list _ -> Null
  | Table (_attr, header, rows) ->
      (* Heavy syntax, no alignment defined yet *)
      let header =
        let row =
          concat_map
            (fun (cell, _align) -> Surround ("{th ", inline cell, "}"))
            header
        in
        concat (Surround ("{tr ", row, "}")) nl
      in
      let rows =
        concat_map
          (fun row ->
            let row =
              concat_map (fun cell -> Surround ("{td ", inline cell, "}")) row
            in
            concat (Surround ("{tr ", row, "}")) nl)
          rows
      in
      BlockSurround ("{table\n", concat header rows, "}")


let of_doc ?(min_head_lvl = 0) doc =
  concat_map (block { min_head_lvl; in_html = false }) doc


let to_string t =
  let buf = Buffer.create 1024 in
  add_to_buffer buf t;
  Buffer.contents buf


let mld_of_md ~min_head_lvl md = to_string @@ of_doc ~min_head_lvl md
