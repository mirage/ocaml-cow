(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

type element = ('a Xml.frag as 'a) Xml.frag
type t = Xml.t

type tree = [ `Data of string | `El of Xmlm.tag * 'a list ] as 'a

let void_elements = [
  "img";
  "input";
  "link";
  "meta";
  "br";
  "hr";
  "source";
  "wbr";
  "param";
  "embed";
  "base";
  "area";
  "col";
  "track";
  "keygen";
]

let doctype = "<!DOCTYPE html>"

let rec generate_signals signals = function
  | `Data s -> (`Data s)::signals
  | `El (tag, children) ->
    let signals = (`El_start tag)::signals in
    let signals = List.fold_left generate_signals signals children in
    match signals with
    | `El_start ((_, tag),_) :: _ when List.mem tag void_elements ->
      `El_end::signals
    | [] | (`Data _ | `Dtd _ | `El_end)::_ -> `El_end::signals
    | `El_start _ :: _ -> `El_end::(`Data "")::signals

let output ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None) dest t =
  let append tree =
    let signals = generate_signals [] tree in
    let out = Xml.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    Xml.output out (`Dtd None);
    List.(iter (Xml.output out) (rev signals))
  in
  List.iter append t

let output_doc ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None) dest t =
  (* This could build an Xmlm.output and use `Dtd to set the DOCTYPE. *)
  let doctype = doctype ^ "\n" in
  begin match dest with
  | `Buffer buf -> Buffer.add_string buf doctype
  | `Channel oc -> output_string oc doctype
  | `Fun f ->
    let len = String.length doctype in
    for i = 0 to len - 1 do f (int_of_char doctype.[i]) done
  end;
  output ~nl ~indent ~ns_prefix dest t

let to_string t =
  let buf = Buffer.create 4096 in
  output_doc (`Buffer buf) t;
  Buffer.contents buf

let of_string ?enc str =
  Xml.of_string ~entity:Xhtml.entity ?enc str

type link = {
  text : string;
  href: string;
}

let html_of_link l : t =
  <:xml<<a href=$str:l.href$>$str:l.text$</a>&>>

let link ?hreflang ?rel ?target ?ty ?title html l =
  let attrs = [(("", "href"), Uri.to_string l)] in
  let attrs = match hreflang with
    | Some h -> (("", "hreflang"), h) :: attrs
    | None -> attrs in
  let attrs = match rel with
    | Some rel ->
       let rel = match rel with
         | `alternate  -> "alternate"
         | `author     -> "author"
         | `bookmark   -> "bookmark"
         | `help       -> "help"
         | `license    -> "license"
         | `next       -> "next"
         | `nofollow   -> "nofollow"
         | `noreferrer -> "noreferrer"
         | `prefetch   -> "prefetch"
         | `prev       -> "prev"
         | `search     -> "search"
         | `tag        -> "tag" in
       (("", "rel"), rel) :: attrs
    | None -> attrs in
  let attrs = match target with
    | Some t ->
       let target = match t with
         | `blank  -> "_blank"
         | `parent -> "_parent"
         | `self   -> "_self"
         | `top    -> "_top"
         | `Frame n -> n in
       (("", "target"), target) :: attrs
    | None -> attrs in
  let attrs = match ty with
    | Some t -> (("", "type"), t) :: attrs
    | None -> attrs in
  let attrs = match title with
    | Some t -> (("", "title"), t) :: attrs
    | None -> attrs in
  `El((("", "a"), attrs), html)

(* color tweaks for lists *)
let interleave classes l =
  let i = ref 0 in
  let n = Array.length classes in
  let get () =
    let res = classes.(!i mod n) in
    incr i;
    res in
  List.map (fun elt -> <:xml< <div class=$str:get ()$>$elt$</div> >>) l

let html_of_string s = <:xml<$str:s$>>
let html_of_int i = <:xml<$int:i$>>
let html_of_float f = <:xml<$flo:f$>>

type table = t array array

let html_of_table ?(headings=false) t =
  let tr x = <:xml<<tr>$list:x$</tr>&>> in
  let th x = <:xml<<th>$x$</th>&>> in
  let td x = <:xml<<td>$x$</td>&>> in
  let hd =
    if Array.length t > 0 && headings then
      let l = Array.to_list t.(0) in
      Some (tr (List.map th l))
    else
      None in
  let tl =
    if Array.length t > 1 && headings then
      List.map Array.to_list (List.tl (Array.to_list t))
    else
      List.map Array.to_list (Array.to_list t) in
  let tl = List.map (fun l -> tr (List.map td l)) tl in
  <:xml<<table>$opt:hd$ $list:tl$</table>&>>

let nil : t = []
