(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2011-2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

include module type of Xmlm

type t = (('a frag as 'a) frag) list

val to_string : ?decl:bool -> t -> string

val of_string :
  ?entity:(string -> string option) ->
  ?enc:encoding ->
  string -> t
(** [of_string s] returns the XML tree described by [s].

    @param entity is called to resolve non predefined entity
    references such as "&amp;".  It must return an UTF-8 string
    corresponding to the replacement character data.  By default, only
    {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
    entities}, i.e., "&lt;", "&gt;", "&amp;", "&apos;", and "&quot;",
    are recognized.

    @param enc The encoding of the document.  Default [None] which
    means that one does not know the encoding. *)
