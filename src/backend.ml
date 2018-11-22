(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Omd_representation
open Omd_utils

(* the follwing to functions come from 
 * https://github.com/ocaml/omd/blob/master/src/omd_backend.ml#L632 *)
let string_of_attrs attrs =
  let b = Buffer.create 1024 in
  List.iter
    (function
      | a, Some v ->
          if not (String.contains v '\'') then Printf.bprintf b " %s='%s'" a v
          else if not (String.contains v '"') then
            Printf.bprintf b " %s=\"%s\"" a v
          else Printf.bprintf b " %s=\"%s\"" a v
      | a, None ->
          (* if html4 then *)
          (*   Printf.bprintf b " %s='%s'" a a *)
          (* else *)
          Printf.bprintf b " %s=''" a
          (* HTML5 *))
    attrs ;
  Buffer.contents b

let filter_text_omd_rev l =
  let rec loop b r = function
    | [] -> if b then r else l
    | ("media:type", Some "text/omd") :: tl -> loop true r tl
    | e :: tl -> loop b (e :: r) tl
  in
  loop false [] l

let rec mld_of_md ~min_header md =
  let quote ?(indent = 0) s =
    let b = Buffer.create (String.length s) in
    let l = String.length s in
    let rec loop nl i =
      if i < l then (
        if nl && i < l - 1 then (
          for _ = 1 to indent do
            Buffer.add_char b ' '
          done ;
          Buffer.add_string b "> " ) ;
        match s.[i] with
        | '\n' ->
            Buffer.add_char b '\n' ;
            loop true (succ i)
        | c ->
            Buffer.add_char b c ;
            loop false (succ i) )
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
  let rec loop ?(fst_p_in_li = true) ?(is_in_list = false) list_indent l =
    (* [list_indent: int] is the indentation level in number of spaces. *)
    (* [is_in_list: bool] is necessary to know if we are inside a paragraph
       which is inside a list item because those need to be indented! *)
    let loop ?(fst_p_in_li = fst_p_in_li) ?(is_in_list = is_in_list)
        list_indent l =
      loop ~fst_p_in_li ~is_in_list list_indent l
    in
    match l with
    | X x :: tl ->
        ( match x#to_t md with
        | Some t -> loop list_indent t
        | None -> (
          match x#to_html ~indent:0 Omd_backend.html_of_md md with
          | Some s -> Printf.bprintf b "{%%html: %s %%}\n" s
          | None -> () ) ) ;
        loop list_indent tl
    | Blockquote q :: tl ->
        Buffer.add_string b
          (quote ~indent:list_indent (mld_of_md ~min_header q)) ;
        if tl <> [] then Buffer.add_string b "\n" ;
        loop list_indent tl
    (* TODO: we need to accumulate the references separately *)
    | Ref (rc, _name, _text, fallback) :: tl ->
        if !references = None then references := Some rc ;
        loop list_indent (Raw fallback#to_string :: tl)
    | Img_ref (rc, _name, _alt, fallback) :: tl ->
        if !references = None then references := Some rc ;
        loop list_indent (Raw fallback#to_string :: tl)
    | Paragraph [] :: tl -> loop list_indent tl
    | Paragraph md :: tl ->
        if is_in_list then
          if fst_p_in_li then add_spaces (list_indent - 2)
          else add_spaces list_indent ;
        loop ~fst_p_in_li:false list_indent md ;
        Printf.bprintf b "\n\n" ;
        loop ~fst_p_in_li:false list_indent tl
    | Img (alt, src, title) :: tl ->
        Printf.bprintf b "{%%html: <img src=\"%s\"%s>%s</img>%%}" src
          (if alt = "" then "" else Printf.sprintf " alt=\"%s\"" alt)
          title ;
        loop list_indent tl
    | Text t :: tl ->
        Printf.bprintf b "%s" (Omd_backend.escape_markdown_characters t) ;
        loop list_indent tl
    | Emph md :: tl ->
        Buffer.add_string b "{e " ;
        loop list_indent md ;
        Buffer.add_string b "}" ;
        loop list_indent tl
    | Bold md :: tl ->
        Buffer.add_string b "{b " ;
        loop list_indent md ;
        Buffer.add_string b "}" ;
        loop list_indent tl
    | Ol l :: tl | Olp l :: tl ->
        if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
        then Buffer.add_char b '\n' ;
        add_spaces list_indent ;
        Buffer.add_string b "{ol \n" ;
        List.iter
          (fun li ->
            add_spaces list_indent ;
            Buffer.add_string b "+ " ;
            loop ~is_in_list:true (list_indent + 4) li ;
            Buffer.add_char b '\n' )
          l ;
        add_spaces list_indent ;
        Buffer.add_string b "}" ;
        if list_indent = 0 then Buffer.add_char b '\n' ;
        loop list_indent tl
    | Ul l :: tl ->
        if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
        then Buffer.add_char b '\n' ;
        add_spaces list_indent ;
        Buffer.add_string b "{ul \n" ;
        List.iter
          (fun li ->
            add_spaces list_indent ;
            Printf.bprintf b "- " ;
            loop ~is_in_list:true (list_indent + 4) li ;
            Buffer.add_char b '\n' )
          l ;
        add_spaces list_indent ;
        Buffer.add_string b "}" ;
        if list_indent = 0 then Buffer.add_char b '\n' ;
        loop list_indent tl
    | Ulp l :: tl ->
        add_spaces list_indent ;
        Buffer.add_string b "{ul \n" ;
        List.iter
          (fun li ->
            if
              Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '\n'
            then Buffer.add_char b '\n' ;
            add_spaces list_indent ;
            Printf.bprintf b "- " ;
            loop ~is_in_list:true (list_indent + 4) li )
          l ;
        add_spaces list_indent ;
        Buffer.add_string b "}" ;
        ( match tl with
        | (H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _) :: _
         |NL :: (H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _) :: _ ->
            Buffer.add_char b '\n'
        | _ -> () ) ;
        loop list_indent tl
    | Code (_lang, c) :: tl ->
        Buffer.add_char b '[' ;
        Printf.bprintf b "%s" c ;
        Buffer.add_char b ']' ;
        loop list_indent tl
    | Code_block (_lang, c) :: tl ->
        Buffer.add_string b "{[\n" ;
        Buffer.add_string b c ;
        if not (Buffer.nth b (Buffer.length b - 1) = '\n') then
          Buffer.add_char b '\n' ;
        Buffer.add_string b "]}\n" ;
        loop list_indent tl
    | Br :: tl -> Buffer.add_string b "\n\n" ; loop list_indent tl
    | Hr :: tl ->
        Buffer.add_string b "{%html: <hr /> %}\n" ;
        loop list_indent tl
    | Raw s :: tl -> Buffer.add_string b s ; loop list_indent tl
    | Raw_block s :: tl ->
        Buffer.add_char b '\n' ;
        Buffer.add_string b s ;
        Buffer.add_char b '\n' ;
        loop list_indent tl
    | Html (tagname, attrs, []) :: tl
      when StringSet.mem tagname html_void_elements ->
        Buffer.add_string b "{%html: " ;
        Printf.bprintf b "<%s" tagname ;
        Buffer.add_string b (string_of_attrs attrs) ;
        Buffer.add_string b " />" ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | Html (tagname, attrs, body) :: tl ->
        Buffer.add_string b "{%html: " ;
        let a = filter_text_omd_rev attrs in
        Printf.bprintf b "<%s" tagname ;
        Buffer.add_string b (string_of_attrs a) ;
        Buffer.add_string b ">" ;
        if a == attrs then loop list_indent body
        else Buffer.add_string b (Omd_backend.html_of_md body) ;
        Printf.bprintf b "</%s>" tagname ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | Html_block (tagname, attrs, body) :: tl ->
        let needs_newlines =
          match tl with
          | NL :: Paragraph p :: _ | Paragraph p :: _ -> p <> []
          | ( H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Ul _ | Ol _ | Ulp _
            | Olp _
            | Code (_, _)
            | Code_block (_, _)
            | Text _ | Emph _ | Bold _ | Br | Hr
            | Url (_, _, _)
            | Ref (_, _, _, _)
            | Img_ref (_, _, _, _)
            | Html (_, _, _)
            | Blockquote _
            | Img (_, _, _) )
            :: _ ->
              true
          | (Html_block (_, _, _) | Html_comment _ | Raw _ | Raw_block _) :: _
            ->
              false
          | X _ :: _ -> false
          | NL :: _ -> false
          | [] -> false
        in
        Buffer.add_string b "{%html: " ;
        if body = [] && StringSet.mem tagname html_void_elements then (
          Printf.bprintf b "<%s" tagname ;
          Buffer.add_string b (string_of_attrs attrs) ;
          Buffer.add_string b " />" ;
          Buffer.add_string b " %}" ;
          if needs_newlines then Buffer.add_string b "\n\n" ;
          loop list_indent tl )
        else
          let a = filter_text_omd_rev attrs in
          Printf.bprintf b "<%s" tagname ;
          Buffer.add_string b (string_of_attrs a) ;
          Buffer.add_string b ">" ;
          if a == attrs then loop list_indent body
          else Buffer.add_string b (Omd_backend.html_of_md body) ;
          Printf.bprintf b "</%s>" tagname ;
          Buffer.add_string b " %}" ;
          if needs_newlines then Buffer.add_string b "\n\n" ;
          loop list_indent tl
    | Html_comment s :: tl ->
        Printf.bprintf b "{%%html: %s %%}" s ;
        loop list_indent tl
    | Url (href, s, _title) :: tl ->
        Printf.bprintf b "{{: %s} %s}" href (mld_of_md ~min_header s) ;
        loop list_indent tl
    | H1 md :: tl ->
        Buffer.add_string b (init_header 0) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | H2 md :: tl ->
        Buffer.add_string b (init_header 1) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | H3 md :: tl ->
        Buffer.add_string b (init_header 2) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | H4 md :: tl ->
        Buffer.add_string b (init_header 3) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | H5 md :: tl ->
        Buffer.add_string b (init_header 4) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | H6 md :: tl ->
        Buffer.add_string b (init_header 5) ;
        loop list_indent md ;
        Buffer.add_string b "}\n" ;
        loop list_indent tl
    | NL :: tl ->
        if
          Buffer.length b = 1
          || Buffer.length b > 1
             && not
                  ( Buffer.nth b (Buffer.length b - 1) = '\n'
                  && Buffer.nth b (Buffer.length b - 2) = '\n' )
        then Buffer.add_string b "\n" ;
        loop list_indent tl
    | [] -> ()
  in
  loop 0 md ;
  ( match !references with
  | None -> ()
  | Some r ->
      Buffer.add_char b '\n' ;
      List.iter
        (fun (name, (url, title)) ->
          Printf.bprintf b "%s: {{: %s} %s}\n" name url
            (if title = "" then url else title) )
        r#get_all ) ;
  let res = Buffer.contents b in
  res
