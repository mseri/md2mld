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
      | ( '!'
        | '*'
        | '\''
        | '('
        | ')'
        | ';'
        | ':'
        | '@'
        | '='
        | '+'
        | '$'
        | ','
        | '/'
        | '?'
        | '%'
        | '#'
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '-' | '_' | '.' | '~' ) as c -> Buffer.add_char b c
      | '&' -> Buffer.add_string b "&amp;"
      | _ as c -> Printf.bprintf b "%%%2X" (Char.code c))
    s;
  Buffer.contents b


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
  | BlockSurround (s1, t1, s2) -> Printf.bprintf buf "\n\b%s%a%s\n" s1 add_to_buffer t1 s2
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
  match s1, s2 with
  | Null, s | s, Null -> s
  | s1, s2 -> Concat (s1, s2)


let concat_map f l = List.fold_left (fun accu x -> concat accu (f x)) Null l

let cross_reference_words =
  [ "module"
  ; "modtype"
  ; "class"
  ; "classtype"
  ; "val"
  ; "type"
  ; "exception"
  ; "attribute"
  ; "method"
  ; "section"
  ; "const"
  ; "recfield"
  ]


let is_cross_reference_regexps =
  cross_reference_words |> List.map (fun w -> Str.regexp_string (w ^ ":"))


let inferred_cross_reference = Str.regexp_string "ref:"

let rec inline ({ il_desc; il_attributes = _ } : Omd.inline) =
  match il_desc with
  | Concat l -> concat_map inline l
  | Text s -> text s
  | Emph il -> Surround ("{e ", inline il, "}")
  | Strong il -> Surround ("{b ", inline il, "}")
  | Code s -> Surround ("[", text s, "]")
  | Hard_break -> text "\n\n"
  | Soft_break -> text "\n"
  | Html body -> Surround ("{%html: ", text body, "%}")
  | Link { label; destination; title = _ } ->
    let cross_reference =
      match label with
      | { il_desc = Text s; _ } when s == destination ->
        if Str.string_match inferred_cross_reference destination 0
        then Some (Str.string_after destination 4)
        else if List.exists
                  (fun r -> Str.string_match r destination 0)
                  is_cross_reference_regexps
        then Some destination
        else None
      | _ -> None
    in
    (match cross_reference with
    | Some cross_reference -> Surround ("{!", text cross_reference, "}")
    | None -> TripleSurround ("{{: ", text destination, "} ", inline label, "}"))
  | Image { label; destination; title } ->
    let img =
      "<img src=\""
      ^ escape_uri destination
      ^ "\" alt=\""
      ^ to_plain_text (inline label)
      ^ "\""
      ^ (match title with
        | None -> ""
        | Some title -> " title=\"" ^ title ^ "\"")
      ^ "/>"
    in
    Surround ("{%html: ", text img, "%}")


let rec block min_head_lvl ({ bl_desc; bl_attributes = _attr } : Omd.block) =
  match bl_desc with
  | Blockquote q -> BlockSurround ("{v ", concat_map (block min_head_lvl) q, "v}")
  | Paragraph md -> GeneralBlock (inline md)
  | List (ty, sp, bl) ->
    let sign =
      match ty with
      | Ordered _ -> "+ "
      | Bullet _ -> "- "
    in
    let li t =
      let block' (t : Omd.block) =
        match t.bl_desc, sp with
        | Paragraph t, Tight -> concat (inline t) nl
        | _ -> block min_head_lvl t
      in
      let nl = if sp = Tight then Null else nl in
      Surround (sign, concat nl (concat_map block' t), "")
    in
    concat nl (concat_map li bl)
  | Code_block (_label, code) -> BlockSurround ("{[\n", text code, "]}")
  | Thematic_break -> GeneralBlock (text "***")
  | Html_block body -> raw body
  | Heading (level, text) ->
    BlockSurround
      ( "{"
        ^ (match level + min_head_lvl with
          | 1 -> "0"
          | 2 -> "1"
          | 3 -> "2"
          | 4 -> "3"
          | 5 -> "4"
          | _ -> "5")
        ^ " "
      , inline text
      , "}" )
  | Definition_list _ -> Null


let of_doc ?(min_head_lvl = 0) doc = concat_map (block min_head_lvl) doc

let to_string t =
  let buf = Buffer.create 1024 in
  add_to_buffer buf t;
  Buffer.contents buf


let mld_of_md ~min_head_lvl md = to_string @@ of_doc ~min_head_lvl md

(* let rec mld_of_md ~min_header md =
  let quote ?(indent = 0) s =
    let b = Buffer.create (String.length s) in
    let l = String.length s in
    let rec loop nl i =
      if i < l
      then (
        if nl && i < l - 1
        then (
          for _ = 1 to indent do
            Buffer.add_char b ' '
          done;
          Buffer.add_string b "> ");
        match s.[i] with
        | '\n' ->
          Buffer.add_char b '\n';
          loop true (succ i)
        | c ->
          Buffer.add_char b c;
          loop false (succ i))
      else Buffer.contents b
    in
    loop true 0
  in
  let b = Buffer.create 64 in
  let add_spaces n =
    for _ = 1 to n do
      Buffer.add_char b ' '
    done
  in
  let init_header lvl = Printf.sprintf "{%d " (min_header + lvl) in
  let references = ref None in
  let rec loop ?(fst_p_in_li = true) ?(is_in_list = false) list_indent (l : Omd.doc) =
    (* [list_indent: int] is the indentation level in number of spaces. *)
    (* [is_in_list: bool] is necessary to know if we are inside a paragraph
       which is inside a list item because those need to be indented! *)
    let loop ?(fst_p_in_li = fst_p_in_li) ?(is_in_list = is_in_list) list_indent l =
      loop ~fst_p_in_li ~is_in_list list_indent l
    in
    match l with
    | { bl_desc = Blockquote q; _ } :: tl ->
      Buffer.add_string b (quote ~indent:list_indent (mld_of_md ~min_header q));
      if tl <> [] then Buffer.add_string b "\n";
      loop list_indent tl
    (* TODO: we need to accumulate the references separately *)
    | { bl_desc = Ref (rc, _name, _text, fallback); _ } :: tl ->
      if !references = None then references := Some rc;
      loop list_indent (Raw fallback#to_string :: tl)
    | { bl_desc = Img_ref (rc, _name, _alt, fallback); _ } :: tl ->
      if !references = None then references := Some rc;
      loop list_indent (Raw fallback#to_string :: tl)
    | { bl_desc = Paragraph []; _ } :: tl -> loop list_indent tl
    | { bl_desc = Paragraph md; _ } :: tl ->
      if is_in_list
      then if fst_p_in_li then add_spaces (list_indent - 2) else add_spaces list_indent;
      loop ~fst_p_in_li:false list_indent md;
      Printf.bprintf b "\n\n";
      loop ~fst_p_in_li:false list_indent tl
    | { bl_desc = Img (alt, src, title); _ } :: tl ->
      Printf.bprintf
        b
        "{%%html: <img src=\"%s\"%s>%s</img>%%}"
        src
        (if alt = "" then "" else Printf.sprintf " alt=\"%s\"" alt)
        title;
      loop list_indent tl
    | { bl_desc = Text t; _ } :: tl ->
      Printf.bprintf b "%s" (Omd_backend.escape_markdown_characters t);
      loop list_indent tl
    | { bl_desc = Emph md; _ } :: tl ->
      Buffer.add_string b "{e ";
      loop list_indent md;
      Buffer.add_string b "}";
      loop list_indent tl
    | { bl_desc = Bold md; _ } :: tl ->
      Buffer.add_string b "{b ";
      loop list_indent md;
      Buffer.add_string b "}";
      loop list_indent tl
    | { bl_desc = Ol l; _ } :: tl | { bl_desc = Olp l; _ } :: tl ->
      if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
      then Buffer.add_char b '\n';
      add_spaces list_indent;
      Buffer.add_string b "{ol \n";
      List.iter
        (fun li ->
          add_spaces list_indent;
          Buffer.add_string b "{+ ";
          loop ~is_in_list:true (list_indent + 4) li;
          Buffer.add_string b "}\n")
        l;
      add_spaces list_indent;
      Buffer.add_string b "}";
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | { bl_desc = Ul l; _ } :: tl | { bl_desc = Ulp l; _ } :: tl ->
      if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
      then Buffer.add_char b '\n';
      add_spaces list_indent;
      Buffer.add_string b "{ul \n";
      List.iter
        (fun li ->
          add_spaces list_indent;
          Buffer.add_string b "{- ";
          loop ~is_in_list:true (list_indent + 4) li;
          Buffer.add_string b "}\n")
        l;
      add_spaces list_indent;
      Buffer.add_string b "}";
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | { bl_desc = Code (_lang, c); _ } :: tl ->
      Buffer.add_char b '[';
      Printf.bprintf b "%s" c;
      Buffer.add_char b ']';
      loop list_indent tl
    | { bl_desc = Code_block (_lang, c); _ } :: tl ->
      Buffer.add_string b "{[\n";
      Buffer.add_string b c;
      if not (Buffer.nth b (Buffer.length b - 1) = '\n') then Buffer.add_char b '\n';
      Buffer.add_string b "]}\n";
      loop list_indent tl
    | { bl_desc = Br; _ } :: tl ->
      Buffer.add_string b "\n\n";
      loop list_indent tl
    | { bl_desc = Hr; _ } :: tl ->
      Buffer.add_string b "{%html: <hr /> %}\n";
      loop list_indent tl
    | { bl_desc = Raw s; _ } :: tl ->
      Buffer.add_string b s;
      loop list_indent tl
    | { bl_desc = Raw_block s; _ } :: tl ->
      Buffer.add_char b '\n';
      Buffer.add_string b s;
      Buffer.add_char b '\n';
      loop list_indent tl
    | { bl_desc = Html (tagname, attrs, []); _ } :: tl
      when StringSet.mem tagname html_void_elements ->
      Buffer.add_string b "{%html: ";
      Printf.bprintf b "<%s" tagname;
      Buffer.add_string b (string_of_attrs attrs);
      Buffer.add_string b " />";
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = Html (tagname, attrs, body); _ } :: tl ->
      Buffer.add_string b "{%html: ";
      let a = filter_text_omd_rev attrs in
      Printf.bprintf b "<%s" tagname;
      Buffer.add_string b (string_of_attrs a);
      Buffer.add_string b ">";
      if a == attrs
      then loop list_indent body
      else Buffer.add_string b (Omd_backend.html_of_md body);
      Printf.bprintf b "</%s>" tagname;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = Html_block (tagname, attrs, body); _ } :: tl ->
      let needs_newlines =
        match tl with
        | { bl_desc = NL; _ } :: Paragraph p :: _ | { bl_desc = Paragraph p; _ } :: _ ->
          p <> []
        | ( H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Ul _ | Ol _ | Ulp _ | Olp _
          | Code (_, _)
          | Code_block (_, _)
          | Text _ | Emph _ | Bold _ | Br | Hr
          | Url (_, _, _)
          | Ref (_, _, _, _)
          | Img_ref (_, _, _, _)
          | Html (_, _, _)
          | Blockquote _
          | Img (_, _, _) )
          :: _ -> true
        | { bl_desc = Html_block (_, _, _) | Html_comment _ | Raw _ | Raw_block _; _ }
          :: _ -> false
        | { bl_desc = X _; _ } :: _ -> false
        | { bl_desc = NL; _ } :: _ -> false
        | [] -> false
      in
      Buffer.add_string b "{%html: ";
      if body = [] && StringSet.mem tagname html_void_elements
      then (
        Printf.bprintf b "<%s" tagname;
        Buffer.add_string b (string_of_attrs attrs);
        Buffer.add_string b " />";
        Buffer.add_string b " %}";
        if needs_newlines then Buffer.add_string b "\n\n";
        loop list_indent tl)
      else (
        let a = filter_text_omd_rev attrs in
        Printf.bprintf b "<%s" tagname;
        Buffer.add_string b (string_of_attrs a);
        Buffer.add_string b ">";
        if a == attrs
        then loop list_indent body
        else Buffer.add_string b (Omd_backend.html_of_md body);
        Printf.bprintf b "</%s>" tagname;
        Buffer.add_string b " %}";
        if needs_newlines then Buffer.add_string b "\n\n";
        loop list_indent tl)
    | { bl_desc = Html_comment s; _ } :: tl ->
      Printf.bprintf b "{%%html: %s %%}" s;
      loop list_indent tl
    | { bl_desc = Url (href, s, _title); _ } :: tl ->
      Printf.bprintf b "{{: %s} %s}" href (mld_of_md ~min_header s);
      loop list_indent tl
    | { bl_desc = H1 md; _ } :: tl ->
      Buffer.add_string b (init_header 0);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = H2 md; _ } :: tl ->
      Buffer.add_string b (init_header 1);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = H3 md; _ } :: tl ->
      Buffer.add_string b (init_header 2);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = H4 md; _ } :: tl ->
      Buffer.add_string b (init_header 3);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = H5 md; _ } :: tl ->
      Buffer.add_string b (init_header 4);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = H6 md; _ } :: tl ->
      Buffer.add_string b (init_header 5);
      loop list_indent md;
      Buffer.add_string b "}\n";
      loop list_indent tl
    | { bl_desc = NL; _ } :: tl ->
      if Buffer.length b = 1
         || (Buffer.length b > 1
            && not
                 (Buffer.nth b (Buffer.length b - 1) = '\n'
                 && Buffer.nth b (Buffer.length b - 2) = '\n'))
      then Buffer.add_string b "\n";
      loop list_indent tl
    | [] -> ()
  in
  loop 0 md;
  (match !references with
  | None -> ()
  | Some r ->
    Buffer.add_char b '\n';
    List.iter
      (fun (name, (url, title)) ->
        Printf.bprintf b "%s: {{: %s} %s}\n" name url (if title = "" then url else title))
      r#get_all);
  let res = Buffer.contents b in
  res *)
