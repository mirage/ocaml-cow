(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Css = struct
  type elt =
    | Str of string
    | Fun of string * expr list

  and expr = elt list

  type prop_decl =
    | Prop of string * expr list
    | Decl of expr list * prop_decl list

  type t =
    | Props of prop_decl list
    | Exprs of expr list

  let props = function
    | Props p -> p
    | _ -> raise Parsing.Parse_error 

  let exprs = function
    | Exprs e -> e
    | _ -> raise Parsing.Parse_error

  let expr = function
    | Exprs [e] -> e
    | _ -> raise Parsing.Parse_error

  let string = function
    | Exprs [[Str s]] -> s
    | _ -> raise Parsing.Parse_error

  module Output = struct

    open Format

    let rec elt ppf (e : elt) = match e with
      | Str s      -> fprintf ppf "%s" s
      | Fun (s,el) -> fprintf ppf "%s(%a)" s exprs el

    and expr ppf (e : expr) = match e with
      | []   -> ()
      | [h]  -> fprintf ppf "%a" elt h
      | h::t -> fprintf ppf "%a %a" elt h expr t

    and exprs ppf (el : expr list) = match el with
      | []   -> ()
      | [h]  -> fprintf ppf "%a" expr h
      | h::t -> fprintf ppf "%a, %a" expr h exprs t

    let rec prop_decl ppf = function
      | Decl (el, pl) -> fprintf ppf "%a {\n%a\n}" exprs el prop_decls pl
      | Prop (n, el)  -> fprintf ppf "\t%s: %a;" n exprs el

    and prop_decls ppf = function
      | []   -> ()
      | h::t -> fprintf ppf "%a\n%a" prop_decl h prop_decls t

    let t ppf (x : t) = match x with
      | Props pl -> prop_decls ppf pl
      | Exprs el -> exprs ppf el

  end

  let to_string t =
    Output.t Format.str_formatter t;
    Format.flush_str_formatter ()

  let is_prop = function
    | Prop _ -> true
    | _      -> false

  let concat_paths p1 p2 = match p1, p2 with
    | [], [] -> []
    | [p],[]
    | [],[p] -> [p]
    | p1,p2  -> List.map (fun e2 -> List.concat (List.map (fun e1 -> e1 @ e2) p1)) p2

  let shift p = function
    | [ Decl (path, body) ] -> Decl (concat_paths p path, body)
    | props                 -> Decl (p, props)

  (* split a root declaration body into a list of prop sequence or decl *)
  let split ps =
    let rec aux current accu = function
    | []               -> List.rev (List.rev current :: accu)
    | (Decl _ as d) :: t -> aux [] ([d] :: List.rev current :: accu) t
    | (Prop _ as p) :: t -> aux (p :: current) accu t in
    List.filter ((<>) []) (aux [] [] ps)

  (* transform a fragment with nested declarations into
     an equivalent fragment with only root declarations *)
  let unroll t =
    let rec aux accu = function
      | Decl (a,b) ->
        if List.for_all is_prop b then
          (* no nested declarations *)
          Decl (a, b) :: accu
        else begin
          (* split/shit/unroll the nested declarations *)
          let splits = split b in
          let shifts = List.map (shift a) splits in
          List.fold_left aux accu shifts
        end
      | x -> x :: accu in
    match t with
      | Props pl -> Props (List.rev (List.fold_left aux [] pl))
      | Exprs er -> assert false
end

type gradient_type = [ `Linear | `Radial ]

let polygradient = function
    | `Radial ->
            let impl ?(behaviour = <:css<circle>>) ?(low = <:css<#0a0a0a>>) ?(high = <:css<#ffffff>>) =
                <:css<
                  background: $low$; /* for non-css3 browsers */
                  background: -webkit-radial-gradient($behaviour$, $low$, $high$);
                  background: -mos-radial-gradient($behaviour$, $low$, $high$);
                  background: -o-radial-gradient($behaviour$, $low$, $high$);
                  background: radial-gradient($behaviour$, $low$, $high$);
                >>
            in impl
    | `Linear ->
            let impl ?(behaviour = <:css<to right>>) ?(low = <:css<#0a0a0a>>) ?(high = <:css<#ffffff>>) =
                let behaviour' = String.lowercase @@ String.trim @@ Css.to_string behaviour in
                let behaviour'' =
                    begin match behaviour' with
                    | "right" -> <:css<to left>>
                    | "left" -> <:css<to right>>
                    | "top" -> <:css<to bottom>>
                    | "bottom" -> <:css<to top>>
                    | "to right" | "to left" | "to top" | "to bottom" -> behaviour
                    end
                in
                <:css<
                  background: $low$;
                  background: -webkit-linear-gradient($behaviour''$, $low$, $high$);
                  background: -moz-linear-gradient($behaviour''$, $low$, $high$);
                  background: -o-linear-gradient($behaviour''$, $low$, $high$);
                  background: linear-gradient($behaviour''$, $low$, $high$);
                >>
            in impl

(* From http://www.webdesignerwall.com/tutorials/cross-browser-css-gradient/ *)
let gradient ?(low = <:css<#0a0a0a>>) ?(high = <:css<#ffffff>>) =
  <:css<
    background: $low$; /* for non-css3 browsers */
    filter: progid:DXImageTransform.Microsoft.gradient(startColorstr=$high$, endColorstr=$low$); /* for IE */
    background: -webkit-gradient(linear, left top, left bottom, from($high$), to($low$)); /* for older webkit browsers */
    background: -moz-linear-gradient(top,  $high$,  $low$); /* for firefox 3.6 to 15 */
    background: -o-linear-gradient(top, $high$, $low$); /* for older versions of Opera (and the sake of completeness) */
 >>

let text_shadow ?(h = <:css<0>>) ?(v = <:css<1px>>) ?(blur = <:css<1px>>) ?(color = <:css<rgba(0,0,0,.3)>>) =
  <:css<
    text-shadow: $h$ $v$ $blur$ $color$;
  >>

let box_shadow ?(h = <:css<0>>) ?(v = <:css<1px>>) ?(blur = <:css<1px>>) ?(color = <:css<rgba(0,0,0,.3)>>) =
  <:css<
    -webkit-box-shadow: $h$ $v$ $blur$ $color$;
    -moz-box-shadow: $h$ $v$ $blur$ $color$;
    box-shadow: $h$ $v$ $blur$ $color$;
  >>

let rounded ?(radius = <:css<.5em>>) =
  <:css<
    -webkit-border-radius: .5em;
    -moz-border-radius: .5em;
    border-radius: .5em;
  >>

let top_rounded =
  <:css<
    -webkit-border-top-left-radius: .5em;
    -webkit-border-top-right-radius: .5em;
    -moz-border-radius-topleft: .5em;
    -moz-border-radius-topright: .5em;
    border-top-left-radius: .5em;
    border-top-right-radius: .5em;
  >>

let bottom_rounded =
  <:css<
    -webkit-border-bottom-left-radius: .5em;
    -webkit-border-bottom-right-radius: .5em;
    -moz-border-radius-bottomleft: .5em;
    -moz-border-radius-bottomright: .5em;
    border-bottom-left-radius: .5em;
    border-bottom-right-radius: .5em;
  >>

let no_padding =
  <:css<
    margin: 0;
    padding: 0;
  >>

let reset_padding =
  <:css<
    html, body, div,
    h1, h2, h3, h4, h5, h6,
    ul, ol, dl, li, dt, dd, p,
    blockquote, pre, form, fieldset,
    table, th, td {
      margin: 0;
      padding: 0;
   }
  >>

include Css
