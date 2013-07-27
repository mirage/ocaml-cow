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

include Xmlm

type t = (('a frag as 'a) frag) list

let id x = x

let to_string ?(decl=false) = function
  | []   -> ""
  | h::t ->
    let buf = Buffer.create 1024 in
    let append decl elt =
      let o = make_output ~decl (`Buffer buf) in
      output o (`Dtd None);
      output_tree id o elt in
    append decl h;
    List.iter (append false) t;
    Buffer.contents buf

(* XXX: do a proper input_subtree integration *)
(*** XHTML parsing (using Xml) ***)
let _input_tree input : t =
  let el (name, attrs) body : t = [ `El ((name, attrs), List.concat body) ] in
  let data str : t = [`Data str] in
  input_tree ~el ~data input

let of_string ?entity ?enc str =
  (* XXX: ugly hack to manually remove the DTD *)
  let remove_dtd str =
    let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" in
    let len = String.length str in
    let decl_len = String.length xml_decl in
    if len >= decl_len && String.sub str 0 decl_len = xml_decl then
      String.sub str decl_len (len - decl_len)
    else
      str in

  (* Here, we want to be able to deal with a forest of possible XML
     trees. To do so correctly, we root the forest with a dummy
     node. *)
  let root str =
    let str = Printf.sprintf "<xxx>%s</xxx>" str in
    let i = make_input ~enc ?entity (`String (0,str)) in
    begin match peek i with
      | `Dtd _ -> let _ = input i in ()
      | _      -> ()
    end;
    match _input_tree i with
    | [`El (_, childs)] -> childs
    | _                 -> raise Parsing.Parse_error in

  (* It is illegal to write <:html<<b>foo</b>>> so we use a small trick
     and write <:html<<b>foo</b>&>> *)
  let remove_trailing_amp str =
    if str.[String.length str - 1] = '&' then
      String.sub str 0 (String.length str - 1)
    else
      str in

  try root (remove_trailing_amp (remove_dtd str))
  with Error (pos, e) ->
    Printf.eprintf "[XMLM:%d-%d] %s: %s\n"(fst pos) (snd pos) str (error_message e);
    raise Parsing.Parse_error
