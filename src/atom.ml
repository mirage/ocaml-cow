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

(* Atom Syndication format output. Bare minimum for a reader to use, feel
   free to extend from the full spec at:
   http://www.atomenabled.org/developers/syndication/atom-format-spec.php
*)
type author = { name : string; uri : string option; email : string option }

let stringo = function None -> None | Some s -> Some (Xml.string s)

let xml_of_author a =
  Xml.(
    tag "name" (string a.name)
    ++ tago "uri" (stringo a.uri)
    ++ tago "email" (stringo a.email))

type date = int * int * int * int * int (* year, month, day, hour, minute *)

let xml_of_date (year, month, day, hour, min) =
  let str =
    Printf.sprintf "%.4d-%.2d-%.2dT%.2d:%.2d:00Z" year month day hour min
  in
  Xml.string str

type link = { rel : [ `self | `alternate ]; href : Uri.t; typ : string option }

let mk_link ?(rel = `self) ?typ href = { rel; typ; href }
let data body : Xml.t = [ `Data body ]
let empty : Xml.t = []

let xml_of_link l =
  let attrs =
    [
      ("rel", match l.rel with `self -> "self" | `alternate -> "alternate");
      ("href", Uri.to_string l.href);
    ]
    @ match l.typ with None -> [] | Some t -> [ ("type", t) ]
  in
  Xml.tag "link" ~attrs empty

type meta = {
  id : string;
  title : string;
  subtitle : string option;
  author : author option;
  rights : string option;
  updated : date;
  links : link list;
}

let xml_of_meta m =
  let open Xml in
  let body =
    [
      tag "id" (data m.id);
      tag "title" (data m.title);
      (match m.subtitle with
      | None -> empty
      | Some s -> tag "subtitle" (data s));
      (match m.author with
      | None -> empty
      | Some a -> tag "author" (xml_of_author a));
      (match m.rights with None -> empty | Some r -> tag "rights" (data r));
      tag "updated" (xml_of_date m.updated);
    ]
  in
  List.concat (body @ List.map xml_of_link m.links)

type content = Xml.t

let xml_of_content base c =
  let div =
    Xml.tag "content"
      ~attrs:[ ("type", "xhtml") ]
      (Xml.tag "div" ~attrs:[ ("xmlns", "http://www.w3.org/1999/xhtml") ] c)
  in
  match base with
  | None -> div
  | Some base -> (
      match div with
      | [ `El ((("", "content"), [ (("", "type"), "xhtml") ]), childs) ] ->
          [
            `El
              ( ( ("", "content"),
                  [ (("", "type"), "xhtml"); (("", "xml:base"), base) ] ),
                childs );
          ]
      | _ -> assert false)

type summary = string option

let xml_of_summary = function
  | None -> Xml.empty
  | Some str -> Xml.(tag "summary" (string str))

type entry = {
  entry : meta;
  summary : summary;
  content : content;
  base : string option;
}

let xml_of_entry e =
  Xml.(
    tag "entry"
      (xml_of_meta e.entry
      ++ xml_of_summary e.summary
      ++ xml_of_content e.base e.content))

let contributors entries =
  List.fold_left
    (fun accu e ->
      match e.entry.author with
      | None -> accu
      | Some a -> if List.mem a accu then accu else a :: accu)
    [] entries

let xml_of_contributor c = Xml.tag "contributor" (xml_of_author c)

type feed = { feed : meta; entries : entry list }

let xml_of_feed ?self f =
  let self =
    match self with
    | None -> Xml.empty
    | Some s -> Xml.tag "link" ~attrs:[ ("rel", "self"); ("href", s) ] Xml.empty
  in
  Xml.(
    tag "feed"
      ~attrs:[ ("xmlns", "http://www.w3.org/2005/Atom") ]
      (self
      ++ xml_of_meta f.feed
      ++ list (List.map xml_of_contributor (contributors f.entries))
      ++ list (List.map xml_of_entry f.entries)))

let compare (yr1, mn1, da1, _, _) (yr2, mn2, da2, _, _) =
  match yr1 - yr2 with
  | 0 -> ( match mn1 - mn2 with 0 -> da1 - da2 | n -> n)
  | n -> n
