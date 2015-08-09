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

let (|>) x f = f x
let (@@) f x = f x

type element = ('a Xml.frag as 'a) Xml.frag
type t = element list

type tree = [ `Data of string | `El of Xmlm.tag * 'a list | `Css of Css.t ] as 'a

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

(** @deprecated *)
type link = {
  text : string;
  href: string;
}

(** @deprecated *)
let html_of_link l : t =
  <:xml<<a href=$str:l.href$>$str:l.text$</a>&>>

let a ?hreflang ?rel ?target ?ty ?title ?cls ~href html =
  let attrs = [(("", "href"), Uri.to_string href)] in
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
  let attrs = match cls with
    | Some c -> (("", "class"), c) :: attrs
    | None -> attrs in
  `El((("", "a"), attrs), html)

let img ?alt ?width ?height ?ismap ?title ?cls src =
  let attrs = [("", "src"), Uri.to_string src] in
  let attrs = match alt with
    | Some t -> (("", "alt"), t) :: attrs
    | None -> attrs in
  let attrs = match width with
    | Some w -> (("", "width"), string_of_int w) :: attrs
    | None -> attrs in
  let attrs = match height with
    | Some h -> (("", "height"), string_of_int h) :: attrs
    | None -> attrs in
  let attrs = match title with
    | Some t -> (("", "title"), t) :: attrs
    | None -> attrs in
  let attrs = match cls with
    | Some c -> (("", "class"), c) :: attrs
    | None -> attrs in
  match ismap with
  | Some u -> a ~href:u ~target:`self
               [`El((("", "img"), (("", "ismap"), "") ::attrs), [])]
  | None -> `El((("", "img"), attrs), [])

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

let concat els =
  List.concat els

let append (_to : t) (el : t) = _to @ el

module Create = struct
  module Tags = struct
    type html_list = [ `Ol of t list | `Ul of t list ]

    type table_flags =
        Headings_fst_col
      | Headings_fst_row
      | Sideways
      | Heading_color of Css.color
      | Bg_color of Css.color

    type 'a table =
      [ `Tr of 'a table list | `Td of 'a * int * int | `Th of 'a * int * int ]
  end

  open Tags

  type t = Xml.t

  let ul ls =
    let els =
      concat (List.map (fun el -> <:html< <li>$el$</li> >>) ls)
    in <:html< <ul>$els$</ul> >>

  let ol ls =
    let els =
      concat (List.map (fun el -> <:html< <li>$el$</li> >>) ls)
    in <:html< <ol>$els$</ol> >>

  let stylesheet css =
    <:html< <style type="text/css">$css:css$</style> >>

  let table ~row ?(flags = [Headings_fst_row]) tbl =
    let h_fst_col = ref false in
    let h_fst_row = ref false in
    let hdg_c = ref (Css.color_of_string "#eDeDeD") in
    let bg_c = ref (Css.color_of_string "#fFfFfF") in
    let side = ref false in
    let () = List.iter (fun tag ->
      match tag with
      | Headings_fst_col -> h_fst_col := true;
      | Headings_fst_row -> h_fst_row := true;
      | Heading_color c -> hdg_c := c;
      | Bg_color c -> bg_c := c;
      | Sideways -> side := true;
      ();)
      flags in
    let rows = List.map row tbl in
    let rows =
      if !side then
        List.mapi (fun i _ -> List.map (fun el -> List.nth el i) rows) @@ List.hd rows
      else
        rows in
    let cellify rows =
      List.map (fun r ->
        List.map (fun el -> <:html<<td>$el$</td>&>>) r
      ) rows in
    let rows =
      match !h_fst_row,!h_fst_col with
      | false,false ->
          cellify rows
      | true,false ->
          let hrow =
            List.hd rows
            |> List.map (fun el -> <:html<<th>$el$</th>&>>) in
          let rest = cellify (List.tl rows) in
          hrow :: rest
      | false,true ->
          List.map (fun r ->
            let h = List.hd r in
            let rest = List.map (fun el -> <:html<<td>$el$</td>&>>) (List.tl r) in
            <:html<<th>$h$</th>&>> :: rest)
            rows
      | true,true ->
          let hrow =
            List.hd rows
            |> List.map (fun el -> <:html<<th>$el$</th>&>>) in
          let rest =
            List.tl rows
            |> List.map (fun r ->
                let hcell = List.hd r in
                let rest = List.flatten @@ cellify [List.tl r] in
                <:html<<th>$hcell$</th>&>> :: rest)
          in hrow :: rest
    in
    let rows = List.map (fun r -> let r = List.flatten r in <:html<<tr>$r$</tr>&>>) rows in
    let rows = concat rows in
    <:html<<table>$rows$</table>&>>

end
