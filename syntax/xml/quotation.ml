(*
 * Copyright (c) 2010-2011 Thomas Gazagnaire <thomas@ocamlpro.com>
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

open Camlp4.PreCast

module Q = Syntax.Quotation
module AQ = Syntax.AntiquotSyntax

let destruct_aq s =
  (* If there is a space, it changes how we interpret antiquote classes. *)
  let space_pos = try Some (String.index s ' ') with Not_found -> None in
  let split pos =
    let len = String.length s in
    let name = String.sub s 0 pos
    and code = String.sub s (pos + 1) (len - pos - 1) in
    name, code
  in
  try
    let pos = String.index s ':' in
    match space_pos with
    | Some spos -> if spos < pos then ("", s) else split pos
    | None -> split pos
  with Not_found ->
    "", s

let parse_quot_string entity _loc s =
  let ast = Xml_parser.parse ?enc:(Xml_parser.get_encoding ()) ?entity _loc s in
  let meta_ast = Qast.meta_t _loc ast in
  meta_ast

let aq_expander =
object
  inherit Camlp4.PreCast.Ast.map as super
  method expr =
    function
      | Camlp4.PreCast.Ast.ExAnt (_loc, s) ->
        let n, c = destruct_aq s in
        let e = AQ.parse_expr _loc c in
        begin match n with
          | "opt"   -> <:expr< match $e$ with [ Some xml -> xml | None -> [] ] >>
          | "int"   -> <:expr< [`Data (string_of_int $e$)] >>
          | "flo"   -> <:expr< [`Data (string_of_float $e$)] >>
          | "str"   -> <:expr< [`Data $e$] >>
          | "uri"   -> <:expr< [`Data (Uri.to_string $e$)] >>
          | "alist" -> <:expr< List.map (fun (k,v) -> (("",k),v)) $e$ >>
          | "list"  -> <:expr< List.concat $e$ >>
          | "attrs" ->

            <:expr<
              let split_key_values str =

              let rec read_key i =
                if i >= String.length str then None
                else match str.[i] with [
                    ' '   -> read_key (i+1)
                  | _     ->
                    try
                      let j = String.index_from str i '=' in
                      Some (j+1, ("",String.sub str i (j-i)))
                    with _ -> None
                  ] in

              let rec read_value i =
                if i >= String.length str then None
                else match str.[i] with [
                    ' '             -> read_value (i+1)
                  | '\'' | '"' as c ->
                    begin try
                      let j = String.index_from str (i+1) c in
                      let str = String.sub str (i+1) (j-i-1) in
                      Some (j+1, str)
                    with _ ->
                      None
                    end
                  | _ -> raise Parsing.Parse_error
                ] in

              let rec aux acc i =
                match read_key i with [
                  None          -> List.rev acc
                | Some (i, key) ->
                  match read_value i with [
                    None  -> raise Parsing.Parse_error
                  | Some x-> aux [(key, snd x)::acc] (fst x)
                  ]
                ] in

              aux [] 0 in

              match $e$ with [
                [`Data str] -> split_key_values str
              | _ -> raise Parsing.Parse_error ] >>

                | "" -> <:expr< $e$ >>
                | x  ->
                  Printf.eprintf "[ERROR] %s is not a valid tag.\nAllowed tags are [opt|int|flo|str|uri|list|alist|attrs] or the empty one." x;
                  Loc.raise _loc Parsing.Parse_error
        end
      | e -> super#expr e
end

let expand_expr entity =
  fun _loc _ s ->
    let meta_ast = parse_quot_string entity _loc s in
    if !Options.needsopen then
      <:expr< let open Cow in ($aq_expander#expr meta_ast$ : Xml.t) >>
    else
      <:expr< ($aq_expander#expr meta_ast$ : Xml.t) >>

let expand_str_item entity _loc _ s =
  let exp_ast = expand_expr entity _loc None s in
  <:str_item< $exp:exp_ast$ >>

let () =
  Q.add "xml" Q.DynAst.expr_tag (expand_expr None);
  Q.add "xml" Q.DynAst.str_item_tag (expand_str_item None)
