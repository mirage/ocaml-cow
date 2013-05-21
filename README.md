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

```
# require "cow";;
# require "cow.syntax";;
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
