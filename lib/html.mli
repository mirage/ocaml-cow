(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = (('a Xml.frag as 'a) Xml.frag) list
(** A sequence of (X)HTML trees. *)

val doctype : string
(**
   @see <http://www.w3.org/TR/html5/syntax.html#the-doctype> The (X)HTML5 DOCTYPE.
*)

val to_string : t -> string
(** [to_string html] is a valid (X)HTML5 polyglot string corresponding
    to the [html] structure.
*)

val of_string : ?enc:Xml.encoding -> string -> t
(** [of_string ?enc html_str] is the tree representation of [html_str]
    as decoded by [enc]. For more information about the default
    encoding, see {!Xmlm.inenc}. *)

val output :
  ?nl:bool ->
  ?indent:int option ->
  ?ns_prefix:(string -> string option) -> Xmlm.dest -> t -> unit
(** Outputs valid (X)HTML5 polyglot text from a {!t}. Only non-void
    element handling is implemented so far.
    For more information about the parameters, see {!Xmlm.make_output}.

    @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
*)

val output_doc :
  ?nl:bool ->
  ?indent:int option ->
  ?ns_prefix:(string -> string option) -> Xmlm.dest -> t -> unit
(** Outputs a valid (X)HTML5 polyglot document from a {!t}. Only
    non-void element handling and HTML5 DOCTYPE is implemented so far.
    For more information about the parameters, see {!Xmlm.make_output}.

    @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup
*)

(** {2 HTML library} *)

type link = {
  text : string;
  href: string;
}

val html_of_link : link -> t

val interleave : string array -> t list -> t list

val html_of_string : string -> t
val html_of_int : int -> t
val html_of_float : float -> t

type table = t array array

val html_of_table : ?headings:bool -> table -> t

val nil : t
