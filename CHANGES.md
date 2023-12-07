### v2.5.0 (10-03-2019)

* Add integrity and crossorigin attributes (#108, @tbrk)
* Dune packaging fixes and updates (#109, #112, @CraiFe, @samoht)

### v2.4.0 (10-03-2019)

* Port to dune fully (#105 @emillon)
* Upgrade opam metadata to 2.0 (@avsm)

### 2.3.0 (29-04-2018)

* Port to jbuilder/dune (@samoht, #102)
* Fix atom feeds (@hannesm, #101)
* Improve the functions generating HTML tags with no content (@Chris00, #100)
* Improve documentation (@Chris00, #99)
* Add a "ty" (type) argument to `Html.link` (@Chris00, #99)
* Update `Html.script` to take a Uri.t for "src" (@Chris00, #99)
* `Html.a`: make ~href optional (also used for anchors) (@Chris00, #99)
* Add the optional argument ?attrs to `Html.a` (@Chris00, #99)
* Use <thead> and <tbody> for table construction (when appropriate) (@Chris00, #99)

### 2.2.0 (15-09-2017)

* Port to use module aliases, so there are now `Cow_xml` `Cow_html`
  `Cow_xhtml` `Cow_markdown` `Cow_json` and  `Cow_atom` modules,
  with aliases to the old scheme under the `Cow` module (e.g. `Cow.Xml`).
  Existing code should continue to work, but the whole compilation unit
  is no longer linked in if just a single method of output is used.
  This bumps the minimum OCaml version to 4.02.3 due to the use of
  module-level aliases.
* Switch build system to use `topkg` instead of `oasis`, and adhere
  to the `opkg` layout format.

### 2.1.0 (21-05-2016)

* Add description lists (dl/dt/dd)
* Add ~licls/~dtcls/~ddcls to Html.ul/ol/dl. Setting classes of child
  elements in lists is sometimes useful.
* Add some missing HTML5 combinators.

### 2.0.1 (03-05-2016)

* Turn off warnings-as-errors, which fixes build under 4.03
* Add OCaml test cases for OCaml 4.03.

### 2.0.0: (13-05-2016)

* Remove camlp4 syntax extension support
* Expose more and clean-up Html combinators

### 1.4.1 (unreleased)

* Fix XML and HTML labeled argument assignment antiquotation syntax bug (#86)
* Fix CSS space-less antiquotation syntax bug

### 1.4.0 (27-09-2015)

* Improve compatability with Type_conv >= 113.00 by renaming some of the
  syntax parser modules to be less generically named.
* Add ocamldoc generation and improve the `Html.Create` library
  (from @chrismamo1 in #82).

### 1.3.0 (02-08-2015)

* Add `Css.of_string`, `Css.set_prop`, `Css.get_prop`, `Css.polygradient`.
  `Css.gradient` (#74, by @chrismamo1)
* Add optional arguments to `Css.top_rounded`, `Css.rounded`, `Css.box_shadow`
  and `Css.text_shadow` (#74, by @chrismamo1)
* Add `Html.concat`, `Html.append`, `Html.Create.ul` and `Html.Create.ol`
  (#74, by @chrismamo1)

### 1.2.2 (30-07-2015)

* Fix int32 conversion to float in JSON syntax (#76, by Antoine Luciani)
* Fix a regression introduced in 1.2.0 in `make test` (#72 by @dsheets)
* Modernize `.travis.yml` to use `ocaml-travisci-skeleton` (by @dsheets)
* Remove direct dependency on re (#71, by @rgrinberg)
* Add a `.merlin` file (#70, by @rgrinberg)

### 1.2.1 (05-04-2015)

* Fix compatibility of the `json` syntax extension with `ezjsonm` version 0.4
  (#68)

### 1.2.0 (06-02-2015)

* When serializing HTML, only self-close void elements.
* New `Html.doctype` value of the HTML5 DOCTYPE.
* New `Html.output` and `Html.output_doc` functions for generic polyglot output.
* Atom support is now deprecated in favor of Syndic
* New `Html.img` constructor for easy creation of <img> tags
* New `Html.a` constructor for easy creation of <a> tags
* Deprecate function `Html.html_of_link` and type `Html.link`

### 1.1.0 (20-12-2014)

* Add OPAM 1.2 compatible description file (#53).
* Fix compatibility with `ezjsonm` version 0.4+ (#55).

### 1.0.0 (26-08-2014)

* Fix OCaml 4.02 compatibility by not exposing a `Location` module
  in syntax extensions to avoid a namespace clash. We now rename them
  to `Xml_location` and `Css_location` and pack those instead.
* Fix BSD compatibility using `$(MAKE)` instead of `make` (since the
  GNU make binary is actually `gmake` on Free/Net/OpenBSD).
* Reduce the verbosity of the build by default.
* Travis: Add OCaml 4.02 and OPAM 1.2.0 tests

### 0.10.1 (10-08-2014)

* Fix Xml.of_string "" invalid argument bug

### 0.10.0 (26-03-2014)

* Remove JSON parsing in favour of using `jsonm` instead.
* Stop testing OCaml 3.12.1 (although it may continue to work).

### 0.9.1 (20-12-2013)

* Fix parsing of empty attributes in XML/HTML/XHTML.

### 0.9.0 (20-12-2013)

* Remove all the Markdown variants except `Omd`, which now claims the
  `Cow.Markdown` module name.
* Clarify the repository license as ISC.
* Run some modules through `ocp-indent`.

### 0.8.1 (15-12-2013)

* Fix META file to include `omd`.
* Improve ocamldoc in CSS module and document quotations in README.
* Add `merlin` editor file.

### 0.8.0 (12-12-2013)

* Add Travis continuous integration scripts.
* Add `Omd_markdown` module based on the `omd` library.
* Note: The `Markdown` and `Markdown_github` modules are now deprecated and will
  be removed before 1.0.

### 0.7.0 (25-09-2013)

* Add an OPAM script that installs the right dependencies.
* Make native dynlink optional if not supported by the toolchain.
* Add support for `<xml:base>` in Atom feeds.

### 0.6.2 (30-08-2013)

* Fix code highlighting of integer literals with underscores.
* Fix XML parsing and printing for fragments and full documents.
* Fix handling of whitespaces in antiquotation attributes.

### 0.6.1 (03-07-2013)

* Tweak CSS syntax highlighting of OCaml code to fit Anil's superior colour
  taste.
* Add a `Code.ocaml_fragment` to get just the syntax highlighted bits without
  the wrapper tags.
* Expose a `decl` option to make the `Xml.to_string` declaration prefix optional.
* Do not output a `<?xml` declaration in `Html.to_string`.

### 0.6.0 (21-06-2013)

* Add URI anti-quotation expander that maps `$uri:u$` to `Uri.to_string u`.
* Xmlm is now an external dependency instead of being bundled with Cow.
* Remove `?templates` from `Html.of_string` and `Xml.of_string`.

### 0.5.5 (12-06-2013)

* Markdown to HTML now generates header titles inside the anchor (previously, a
  space was anchored).

### 0.5.4 (21-05-2013)

* Add `json_of` and `of_json` type-conv providers (#6).
* Add `Json.to_string_hum` which adds more newlines for human-readable output
  (#7).
* Document the `-cow-no-open` camlp4 flag (#9).
* Be compatible with Core by using `List.concat` instead of `List.flatten` (#2).
* Fix Atom feed by adding a `link` field.
* Install `.mli` files with the library binaries (#11).
* New dependency on the `Uri` library.

### 0.5.3 (18-02-2013)

* Correct META file for `cow.syntax` to not repeat the `Str` module loading.
* Fix error with unary sum types for HTML generation.
* Support JSON marshalling for `option` types.

### 0.5.2 (15-01-2013)

* Remove dependency towards the Str module

### 0.5.1 (10-01-2013)

* Fix bugs in `Markdown_github` and factorize html serialization functions
* New antiquotation kinds for CSS
* Add xhtml quotations (which are identical to html from now on)

### 0.5.0 (04-12-2012)

* Remove the broken `Twitter` module
* Add `Markdown_github` to read Github's markdown files

### 0.4.0 (05-10-2012)

* Support (and require) type_conv-108.07.00+

### 0.3.2 (11-09-2012)

* Add `<:html<` quotation, based on the XML generator,
  but with all the XHTML entities also supplied.

### 0.3.1 (04-09-2012)

* Fix the OCamlfind META file.

### 0.3.0 (02-09-2012)

* Initial public release.
