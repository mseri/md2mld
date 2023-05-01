
## 0.7 (2023-05-01)
- Add option to dump OMD ast as sexp for debugging ([#20](https://github.com/mseri/md2mld/pull/20) MisterDA)
- Support code block language tags ([#19](https://github.com/mseri/md2mld/pull/19) MisterDA)
- Add support for odoc heavy table syntax ([#15](https://github.com/mseri/md2mld/pull/15) MisterDA)


## 0.6 (2022-10-03)
- Add support for `-version` option ([#10](https://github.com/mseri/md2mld/pull/10) dmbaturin)
- Escape square brackets, braces and `@` characters in text nodes, fixes [#4](https://github.com/mseri/md2mld/issues/4) ([#9](https://github.com/mseri/md2mld/pull/9) dmbaturin)
- Add support for reading Markdown from the standard input ([#8](https://github.com/mseri/md2mld/pull/8) dmbaturin)

## 0.5.1 (2021-06-25)
- improve generation of (un)ordered lists

## 0.5.0 (2021-06-25)
- port to omd 2.0.0~alpha2

## 0.4.0 (2021-06-25)
- use html blockquotes
- port to omd 2.0.0~alpha1 -- including cross-linking features (from https://github.com/ocaml/omd/pull/215)
- trim output
- modified argument parsing

## 0.3.0 (2018-11-23)
- test using dune promotion behaviour
- re-format using ocamlformt
- allow the minimal header section value to be specified on the CLI
- fix bug on conversion of ordered and unordered list items

## 0.2.0 (2018-11-22)
- port to dune
- updated README

## 0.1.1 (2018-10-24)
- minor code cleanups

## 0.1.0 (2018-06-15)
- first release
