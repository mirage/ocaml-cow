Writing web-applications requires a lot of skills: HTML, CSS, XML, JSON and
Markdown, to name but a few!  This library provides OCaml syntax extensions for
these web formats by:

* extending standard OCaml syntax with embedded web DSLs. It has a
  quotation mechanism which parses HTML, CSS or XML to OCaml, and
  also anti-quotations that form a template mechanism.

* using type-driven code generation to generate markup directly from
  OCaml type declarations. It is possible to mix hand-written and
  generated code to deal with special-cases.  Most of the work is
  done at pre-processing time, so there is no runtime costs and the
  generated OCaml code can be manually inspected if desired.

See more explanation at: http://www.openmirage.org/wiki/cow

This library is in beta, and full documentation is still being written.
Some repositories which use it include:

* Mirage website: http://github.com/mirage/mirage-www
* Opam2web: http://github.com/OCamlpro/opam2web

Usage
-----

Cow installs two ocamlfind packages:

* `cow` which is a packed `Cow` module that contains JSON, XML, etc.
* `cow.syntax` which has the camlp4 support.

The syntax extension assumes that `Cow` is opened in the environment 
where you use the quotations, but you can pass `-cow-no-open` to in the camlp4
command-line to prevent this behaviour.  This is for legacy compatibility
reasons, and we may change the default behaviour before the 1.0 release.

Syntax extension
----------------

This provides the following type-conv providers:

```ocaml
# #use "topfind";;
# #camlp4o;;
# #require "cow";;
# #require "cow.syntax";;
# open Cow
# type t = { foo: int; bar: string } with json ;;
type t = { foo : int; bar : string; }
val json_of_t : t -> Cow.Json.t = <fun>
val t_of_json : Cow.Json.t -> t = <fun>

# type t = { foo: int; bar: string } with of_json ;;
type t = { foo : int; bar : string; }
val t_of_json : t -> Cow.Json.t = <fun>

# type t = { foo: int; bar: string } with json_of ;;
type t = { foo : int; bar : string; }
val json_of_t : t -> Cow.Json.t = <fun>

# type t = { foo: int; bar: string } with xml ;;
type t = { foo : int; bar : string; }
val xml_of_t : t -> Cow.Xml.t = <fun>

# type t = { foo: int; bar: string } with html ;;
type t = { foo : int; bar : string; }
val html_of_t : ?id:string -> t -> Cow.Html.t = <fun>
```

It also provides the follow quotation expanders. In a toplevel such as 
`utop`, activate the syntax extension and:

```ocaml
# #require "cow.syntax";;
# <:xml< <foo>bar</foo> >>
- : Cow.Xml.t = [`Data " "; `El ((("", "foo"), []), [`Data "bar"]); `Data " "]
#  <:xml< <foo>bar</foo>&>>;;
- : Cow.Xml.t = [`Data " "; `El ((("", "foo"), []), [`Data "bar"])]
```

Note the second example terminates the quotation with `&>>` which prevents a
trailing space from showing up.  This is required since `>>>` would be a syntax
error in `camlp4`.

There are also quotation expanders available for `<:xhtml< >>`, `<:css< >>`,
and `<:html< >>` which let you construct values of their respective types by
directly entering them in their native syntaxes.

Use `$type:expr$` to evaluate `expr` and insert it into a quotation:

```ocaml
# let item x = <:html< <li>$str:x$</li> >>
  and klass = "items" in
  <:html<
    <ul class=$str:klass$>
      $list:List.map ["foo";"bar"] item$
    </ul>
  >>
  |> Xml.to_string
  |> print_string;;

  <ul class="items">
     <li>foo</li>  <li>bar</li> 
  </ul>
  - : unit = ()
```

HTML
----

The HTML library is actually an XML-based one (based on xmlm:
http://erratique.ch/software/xmlm).  You should be careful to use it for
fragments of HTML, but you may hit various limitations with HTML input parsing.
We hope to solve this before Cow 1.0, but it isn't there yet...

Markdown
--------

The Markdown library used is based on the [Omd](https://github.com/pw374/omd)
library and tries to be as faithful to the Markdown standard as possible.

JSON
----

The JSON library used is based on [Jsonm](http://erratique.ch/software/jsonm),
with a higher-level set of combinators via the
[Ezjsonm](https://github.com/samoht/ezjsonm) library.
