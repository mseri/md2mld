(lang dune 3.7)
(name md2mld)

(generate_opam_files true)

(license "ISC")
(source (github mseri/md2mld))
(documentation "https://mseri.github.io/md2mld/")
(authors "Marcello Seri <marcello.seri@gmail.com>")
(maintainers "Marcello Seri <marcello.seri@gmail.com>")

(package
 (name md2mld)
 (synopsis "Little cli tool to convert md files into mld files")
 (depends
  (ocaml (>= "4.08"))
  base-bytes
  (omd (>= "2.0.0~alpha3"))))
