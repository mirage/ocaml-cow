(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

(** The Atom Syndication format. See RFC4287 for the full specification *)

type author = {
  name  : string;
  uri   : string option;
  email : string option;
}

(** year, month, date, hour, minute *)
type date = int * int * int * int * int

val compare : date -> date -> int

(** An Atom URI. There are lots of rules on which combinations of links
    are permitted in one feed. See RFC4287 Sec 4.1.1 for the gory details.
  *)
type link = {
  rel : [`self|`alternate];
  href: Uri.t;
  typ : string option;
}

(** [mk_link ~rel ~typ uri] builds a {!link}. [rel] defaults to [`self],
    and [typ] represents the optional MIME type (e.g. [text/html]).
    The [uri] should usually be a fully qualified URI. *)
val mk_link : ?rel:[`self|`alternate] -> ?typ:string -> Uri.t -> link

type meta = {
  id      : string;
  title   : string;
  subtitle: string option;
  author  : author option;
  rights  : string option;
  updated : date;
  links   : link list;
}

type summary = string option

(** A single entry in the Atom feed.  The [base] represents the base
    href for the contents of the feed, in case it has relative links. *)

type entry = {
  entry   : meta;
  summary : summary;
  content : Xml.t;
  base    : string option;
}

type feed = {
  feed    : meta;
  entries : entry list;
}

val xml_of_feed : ?self:string -> feed -> Xml.t
