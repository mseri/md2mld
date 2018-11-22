# Convert md files into odoc mld files

md2mld converts a Markdown-format file into the `mld` format used by [odoc](https://github.com/ocaml/odoc) to render HTML documentation or OCaml libraries.  You can use this script to automatically embed a README.md file into API documentation for an OCaml  library.

You can use it manually as follows

```
$ md2mld filename.md > outfile.mld
```

In `dune` you can use it to generate an mld file with

```
(rule (with-stdout-to outfile.mld (run md2mld filename.md)))
```

