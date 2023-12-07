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

(** (X)HTML library *)

type t = Xml.t
(** A sequence of (X)HTML trees. *)

val doctype : string
(** @see <http://www.w3.org/TR/html5/syntax.html#the-doctype> The
    (X)HTML5 DOCTYPE. *)

val to_string : t -> string
(** [to_string html] is a valid (X)HTML5 polyglot string corresponding
    to the [html] structure. *)

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
  ?ns_prefix:(string -> string option) ->
  Xmlm.dest ->
  t ->
  unit
(** Outputs valid (X)HTML5 polyglot text from a {!t}. Only non-void
    element handling is implemented so far. For more information
    about the parameters, see {!Xmlm.make_output}.

    @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup *)

val output_doc :
  ?nl:bool ->
  ?indent:int option ->
  ?ns_prefix:(string -> string option) ->
  Xmlm.dest ->
  t ->
  unit
(** Outputs a valid (X)HTML5 polyglot document from a {!t}. Only
    non-void element handling and HTML5 DOCTYPE is implemented so far.
    For more information about the parameters, see
    {!Xmlm.make_output}.

    @see <http://www.w3.org/TR/html-polyglot/> Polyglot Markup *)

(** {2 HTML library} *)

type rel =
  [ `alternate
  | `author
  | `bookmark
  | `help
  | `license
  | `next
  | `nofollow
  | `noreferrer
  | `prefetch
  | `prev
  | `search
  | `tag ]

type target = [ `blank | `parent | `self | `top | `Frame of string ]

val a :
  ?cls:string ->
  ?attrs:(string * string) list ->
  ?hreflang:string ->
  ?rel:rel ->
  ?target:target ->
  ?ty:string ->
  ?title:string ->
  ?href:Uri.t ->
  t ->
  t
(** [a href html] generate a link from [html] to [href].

    @param title specifies extra information about the element that is
    usually as a tooltip text when the mouse moves over the element.
    Default: [None].

    @param target Specifies where to open the linked document.

    @param rel Specifies the relationship between the current document
    and the linked document.  Default: [None].

    @param hreflang the language of the linked document.  Default:
    [None].

    @param ty Specifies the media type of the linked document.  *)

type cors = [ `anonymous | `use_credentials ]
(** Cross Origin Resource Sharing (CORS)

    @see <https://developer.mozilla.org/en-US/docs/Web/HTML/CORS_settings_attributes> The crossorigin attribute
 *)

val img :
  ?alt:string ->
  ?width:int ->
  ?height:int ->
  ?ismap:Uri.t ->
  ?title:string ->
  ?cls:string ->
  ?crossorigin:cors ->
  ?attrs:(string * string) list ->
  Uri.t ->
  t

val interleave : string array -> t list -> t list

val html_of_string : string -> t
(** @deprecated use {!string} *)

val string : string -> t

val html_of_int : int -> t
(** @deprecated use {!int} *)

val int : int -> t

val html_of_float : float -> t
(** @deprecated use {!float} *)

val float : float -> t

type table = t array array

val html_of_table : ?headings:bool -> table -> t

val nil : t
(** @deprecated use {!empty} *)

val empty : t

val concat : t list -> t
(** @deprecated use {!list} *)

val list : t list -> t
val some : t option -> t

val append : t -> t -> t
(** [append par ch] appends ch to par *)

val ( ++ ) : t -> t -> t

module Create : sig
  module Tags : sig
    type html_list = [ `Ol of t list | `Ul of t list ]
    type color = Rgba of char * char * char * char | Rgb of char * char * char

    type table_flags =
      | Headings_fst_col
      | Headings_fst_row
      | Sideways
      | Heading_color of color
      | Bg_color of color

    type 'a table =
      [ `Tr of 'a table list | `Td of 'a * int * int | `Th of 'a * int * int ]
  end

  type t = Xml.t

  val stylesheet : string -> t
  (** [stylesheet style] converts a COW CSS type to a valid HTML stylesheet *)

  val table : ?flags:Tags.table_flags list -> row:('a -> t list) -> 'a list -> t
  (** [table ~flags:f ~row:r tbl] produces an HTML table formatted according to
      [f] where each row is generated by passing a member of [tbl] to [r].

      @param flags a list of type [Html.Flags.table_flags] specifying how
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
let table = Html.Create ~flags:[Headings_fst_row] ~row data
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

(** {1 HTML nodes} *)

type node = ?cls:string -> ?id:string -> ?attrs:(string * string) list -> t -> t
(** The type for nodes. *)

val tag : string -> node
(** [tag name t] returns [<name>t</name>] where [<name>] can have
    attributes "class" (if [cls] is given), "id" (if [id] is given)
    and other attributes specified by [attrs].  You are encouraged not
    to use [tag] but prefer the specialized versions below whenever
    possible. *)

val div : node
(** [div ~cls:"cl" t] is [<div class="cl">t</div>]. *)

val span : node
(** [div ~cls:"cl" t] is [<div class="cl">t</div>]. *)

val input :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?ty:string ->
  string ->
  t
(** [input v] returns a button with value "v".
    @param ty the type of the input.  Default: ["button"]. *)

val br : t
val hr : t

val wbr : t
(** A "Word Break Opportunity" node. *)

val param : name:string -> string -> t
(** [param name value] return a [<param>] node to be used in [<object>]. *)

val embed :
  ?width:int ->
  ?height:int ->
  ?ty:string ->
  ?attrs:(string * string) list ->
  Uri.t ->
  t
(** [embed uri] returns an [<embed>] node for [uri]. *)

val col :
  ?cls:string -> ?style:string -> ?attrs:(string * string) list -> int -> t
(** [col n] return a <col span="[n]"/> tag to specify properties of
    columns in a <colgroup>. *)

val source : ?media:string -> ?ty:string -> Uri.t -> t
(** [source uri] returns a <source> tag to be used in an <audio> or
    <video> tag.  It specifies an alternative location [uri] and its
    type [ty] for the browser to choose from. *)

val track :
  ?default:bool ->
  ?label:string ->
  [ `Captions | `Chapters | `Descriptions | `Metadata | `Subtitles of string ] ->
  Uri.t ->
  t
(** [track uri] returns a <track> node to insert in an <audio> or
    <video> tag.  The argument of [`Subtitles] is the language of the
    track. *)

val keygen :
  ?autofocus:bool ->
  ?disabled:bool ->
  ?form:string ->
  ?challenge:bool ->
  ?keytype:[ `RSA | `DSA | `EC ] ->
  string ->
  t
(** [keygen name] return a <keygen> tag that specifies a key-pair
    generator field used for forms. *)

val anchor : string -> t
val h1 : node
val h2 : node
val h3 : node
val h4 : node
val h5 : node
val h6 : node
val li : node
val dt : node
val dd : node

val ul :
  ?add_li:bool ->
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?licls:string ->
  t list ->
  t

val ol :
  ?add_li:bool ->
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?licls:string ->
  t list ->
  t

val dl :
  ?add_dtdd:bool ->
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?dtcls:string ->
  ?ddcls:string ->
  (t * t) list ->
  t

val p : node

val blockquote :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?cite:Uri.t ->
  t ->
  t

val pre : node

val figure :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?figcaption:t ->
  t ->
  t

val main : node

val s : node
(** The [<s>] tag specifies text that is no longer correct, accurate
    or relevant.  The [<s>] tag should not be used to define replaced
    or deleted text, use the [<del>] for that purpose.  *)

val cite : node

val q :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?cite:Uri.t ->
  t ->
  t

val dfn :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?title:string ->
  t ->
  t

val abbr :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?title:string ->
  t ->
  t

val data :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  value:string ->
  t ->
  t

val time :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?datetime:string ->
  t ->
  t

val code : node
val sub : node
val sup : node
val b : node
val u : node
val mark : node
val bdi : node
val bdo : node
val ruby : node
val rb : node
val rt : node
val rtc : node
val rp : node
val aside : node

val ins :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?cite:Uri.t ->
  ?datetime:string ->
  t ->
  t

val del :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?cite:Uri.t ->
  ?datetime:string ->
  t ->
  t

val html : node
val footer : node
val title : node
val header : node
val body : node
val nav : node
val section : node
val article : node
val address : node

val script :
  ?src:Uri.t ->
  ?ty:string ->
  ?charset:string ->
  ?integrity:string ->
  ?crossorigin:cors ->
  t ->
  t

(* val map : name:string -> t -> t *)

(* type area_shape = [ *)
(*   | `Rect of int * int * int * int *)
(*   | `Circle of int * int * int *)
(*   | `Poly of (int * int) list ] *)

(* val area : ?download:string -> ?ty:string -> *)
(*            area_shape -> ?target:string -> Uri.t -> t *)

(** {2 Head elements} *)

val head : node

val link :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?title:string ->
  ?media:string ->
  ?ty:string ->
  ?rel:string ->
  ?integrity:string ->
  ?crossorigin:cors ->
  Uri.t ->
  t
(** [link uri] returns a <link href="[uri]"> element to be put in the
    <head>. *)

val meta :
  ?cls:string ->
  ?id:string ->
  ?name:string ->
  ?content:string ->
  ?charset:string ->
  (string * string) list ->
  t
(** [meta attrs] returns a <meta> tag to be put in the <head>. *)

val base :
  ?cls:string ->
  ?id:string ->
  ?attrs:(string * string) list ->
  ?target:string ->
  Uri.t ->
  t
(** [base uri] returns a <base href="[uri]" /> tag that specifies
    the base URI for all relative URLs in the HTML document.  *)

val style : ?media:string -> ?scoped:bool -> string -> t
(** [style css] return a <style> tag giving the [css] directives.
    This tag is typically found in the <head>.  In the <body> of the
    document, [scoped] must be set to [true].

    @param scoped Specifies that the styles only apply to this
           element's parent element and that element's child elements.
           Only for HTML5.  Default: [false].
    @param media Specifies what media/device the media resource is
           optimized for. *)

(** {2 Discouraged HTML tags}

    Most of the tags below are not deprecated in HTML5 but are
    discouraged in favor of using CSS stylesheets.  *)

val small : node

val i : node
(** In HTML5, it is not guaranteed that it will render text in italics.
    @deprecated Use CSS instead. *)

val tt : node
(** The [<tt>] tag is {i not} supported in HTML5.
    @deprecated Use CSS instead. *)

val em : node
val strong : node
val var : node
val kbd : node
val samp : node
