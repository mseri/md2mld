# md2mld: Convert md files into odoc mld files

`md2mld` converts a Markdown-format file into the `mld` format used by [odoc](https://github.com/ocaml/odoc) to render HTML documentation or OCaml libraries.  You can use this script to automatically embed a `README.md` file into API documentation for an OCaml  library.

## Usage

You can use it manually as follows

```console
$ md2mld filename.md > outfile.mld
```

In `dune` you can use it to generate an mld file with

```text
(rule
 (target outfile.mld)
 (deps filename.md)
 (action
  (with-stdout-to outfile.mld (run md2mld filename.md))))
```

Attach the mld file using the [`(documentation …)` stanza](https://dune.readthedocs.io/en/stable/documentation.html#documentation-stanza-examples).
You can see the documentation generated from the latest tagged version of this README at [mseri.github.io/md2mld/md2mld](https://mseri.github.io/md2mld/md2mld).


## Known issues

- Until the new odoc [fixing #141](https://github.com/ocaml/odoc/issues/141) is released, the minimal header allowed in the `md` file will be the level 3 one `###`.
You can work around this by using the `-min-header 3` flag during the invocation of `md2mld`.

- If you see an error like `'{0': heading level should be lower than top heading level '0'`, this is because in `ocamldoc` the first header must have a level higher than all other headings in the page.
You can safely ignore it or increase the level of the subsequent headings to get rid of it.
