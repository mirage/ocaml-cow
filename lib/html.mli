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

type element = 'a Xml.frag constraint 'a = element
(** A (X)HTML tree. *)

type t = element list
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
    encoding, see {!Xmlm.inenc}.

    Note that this function converts all
    {{:https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references}
    standard entities} into their corresponding UTF-8 symbol. *)

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

(** @deprecated *)
type link = {
  text : string;
  href: string;
  (** The URI of the link.  You must take care of properly
      percent-encode the URI. *)
}

val html_of_link : link -> t
(** @deprecated Use [a] instead. *)

val a : ?hreflang: string ->
        ?rel: [ `alternate | `author | `bookmark | `help | `license
              | `next | `nofollow | `noreferrer | `prefetch
              | `prev | `search | `tag ] ->
        ?target: [ `blank | `parent | `self | `top | `Frame of string ] ->
        ?ty: string ->
        ?title: string ->
        ?cls: string ->
        href:Uri.t -> t -> element
(** [a href html] generate a link from [html] to [href].

    @param title specifies extra information about the element that is
                 usually as a tooltip text when the mouse moves over
                 the element.  Default: [None].
    @param target Specifies where to open the linked document.
    @param rel Specifies the relationship between the current document
               and the linked document.  Default: [None].
    @param hreflang the language of the linked document.  Default: [None].
    @param ty Specifies the media type of the linked document.  *)

val img : ?alt: string ->
          ?width: int ->
          ?height: int ->
          ?ismap: Uri.t ->
          ?title: string ->
          ?cls: string ->
          Uri.t -> element

val interleave : string array -> t list -> t list

val html_of_string : string -> t
val html_of_int : int -> t
val html_of_float : float -> t

type table = t array array

val html_of_table : ?headings:bool -> table -> t

val nil : t

(** [concat els] combines all the members of [els] into a single [html.t]
 * @param els a list of the elements to combine *)
val concat : t list -> t

(** [append par ch] appends ch to par *)
val append : t -> t -> t

module Create : sig
  module Tags : sig
    type html_list = [`Ol of t list | `Ul of t list]

    type table_flags =
        Headings_fst_col
      | Headings_fst_row
      | Sideways
      | Heading_color of Css.color
      | Bg_color of Css.color 

    type 'a table =
      [ `Tr of 'a table list | `Td of 'a * int * int | `Th of 'a * int * int ]
  end

  type t = Xml.t

  val ul : t list -> t

  val ol : t list -> t
  (** [ul ls] converts an OCaml list of HTML elements to a valid HTML ordered
   *  list *)

  val stylesheet : Css.t -> t
  (** [stylesheet style] converts a COW CSS type to a valid HTML stylesheet *)

  val table :
    row:('a -> t list) ->
    ?flags:Tags.table_flags list ->
    'a list ->
    t
  (** [table ~flags:f ~row:r tbl] produces an HTML table formatted according to
      [f] where each row is generated by passing a member of [tbl] to [r].

      @param flags a list of type [Cow.Html.Flags.table_flags] specifying how
      the generated table is to be structured.
      
      @param row a function to transform a single row of the input table (a
      single element of the list, that is) into a list of elements, each of
      which will occupy a cell in a row of the table.
      
      [tbl:] a list of (probably) tuples representing a table.

      See the following example:
{[
let row = (fun (name,email) -> [ <:html<$str:name$>>; <:html<$str:email$>>]) in
let data =
  \[ "Name","Email Address";
    "John Christopher McAlpine","christophermcalpine\@gmail.com";
    "Somebody McElthein","johnqpublic\@something.something";
    "John Doe","johndoe\@johndoe.com"; \] in
let table = Cow.Html.Create ~flags:[Headings_fst_row] ~row data
]}
      which produces the HTML table
{%html:
<!DOCTYPE html>
<table>
  <tr>
    <th>Name</th>                       <th>Email Address</th>
  </tr>
  <tr>
    <td>John Christopher McAlpine</td>  <td>christophermcalpine\@gmail.com</td>
  </tr>
  <tr>
    <td>Somebody McElthein</td>         <td>johnqpublic\@something.something</td>
  </tr>
  <tr>
    <td>John Doe</td>                   <td>johndoe\@johndoe.com</td>
  </tr>
</table>
%}
*)
end
