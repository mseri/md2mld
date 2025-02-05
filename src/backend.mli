(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val mld_of_md : min_head_lvl:int -> Omd.doc -> string
(** [mld_of_md ~min_head_lvl md] returns a string containing the mld version of
    [md]. Note that [md] uses the {!Omd} internal representation of
    Markdown. The ~min_head_lvl parameters allows you to choose the value
    for the section heading associated to <h1>, all the other values will
    be determined incrementally. *)
