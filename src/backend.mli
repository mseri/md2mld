(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(** [mld_of_md ~min_header md] returns a string containing the mld version of
    [md]. Note that [md] uses the {!Omd} internal representation of
    Markdown. The ~min_header parameters allows you to choose the value
    for the section heading associated to <h1>, all the other values will
    be determined incrementally. *)
val mld_of_md : min_header:int -> Omd_representation.t -> string
