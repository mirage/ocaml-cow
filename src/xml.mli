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

(** XML combinators. *)

include module type of Xmlm

type t = ('a frag as 'a) frag list
(** The type for XML fragments. *)

val to_string : ?decl:bool -> t -> string

val of_string :
  ?entity:(string -> string option) -> ?enc:encoding -> string -> t
(** [of_string s] returns the XML tree described by [s].

    @param entity is called to resolve non predefined entity
    references such as "&amp;".  It must return an UTF-8 string
    corresponding to the replacement character data.  By default, only
    {{:http://www.w3.org/TR/REC-xml/#sec-predefined-ent}predefined
    entities}, i.e., "&lt;", "&gt;", "&amp;", "&apos;", and "&quot;",
    are recognized.

    @param enc The encoding of the document.  Default [None] which
    means that one does not know the encoding. *)

(** {1 Combinators} *)

val empty : t
(** [empty] is the empty XML fragment. *)

val string : string -> t
(** [string s] is the XML fragment [s]. *)

val int : int -> t
(** [int i] is the XML fragment [i]. *)

val float : float -> t
(** [float f] is the XML fragment [f]. *)

val list : t list -> t
(** [list xs] is the XML fragment [x1 ... xn]. *)

val some : t option -> t
(** [some t] is [t] if it's not empty, {!empty} otherwise. *)

val uri : Uri.t -> t
(** [uri t] is [t]. *)

val tag : string -> ?attrs:(string * string) list -> t -> t
(** [tag k v] is [<k>v</k>] *)

val tago : string -> ?attrs:(string * string) list -> t option -> t
(** [tago k v] is [k v] if [v] is not [None], otherwise it's {!empty}. *)

val ( ++ ) : t -> t -> t
(** [x ++ y] is [x y] *)
