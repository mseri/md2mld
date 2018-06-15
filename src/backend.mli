(***********************************************************************)
(* ml2mld: convert Markdown files to mld for odoc                      *)
(* (c) 2018 by Marcello Seri <marcello.seri@gmail.com>                 *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val mld_of_md : Omd_representation.t -> string
(** [mld_of_md md] returns a string containing the mld version of
    [md]. Note that [md] uses the {!Omd} internal representation of
    Markdown. *)
