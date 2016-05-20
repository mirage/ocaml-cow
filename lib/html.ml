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

let (@@) f x = f x

let (|>) x f = f x

type t = Xml.t

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

type node = ?cls:string -> ?id:string -> ?attrs:(string * string) list -> t -> t

let tag name ?cls ?id ?(attrs=[]) t =
  let attrs = match id with
    | None   -> attrs
    | Some i -> ("id", i) :: attrs
  in
  let attrs = match cls with
    | None   -> attrs
    | Some c -> ("class", c) :: attrs
  in
  Xml.tag name ~attrs t

let div = tag "div"
let span = tag "span"
let input = tag "input"
let meta = tag "meta"
let br = tag "br"
let hr = tag "hr"
let source = tag "source"
let wbr = tag "wbr"
let param = tag "param"
let embed = tag "embed"
let base = tag "base"
let col = tag "col"
let track = tag "track"
let keygen = tag "keygen"
let footer = tag "footer"
let header = tag "header"
let head = tag "head"
let title = tag "title"
let body = tag "body"
let nav = tag "nav"
let tr = tag "tr"
let th = tag "th"
let td = tag "td"
let link = tag "link"
let section = tag "section"

let empty: t = []
let list = List.concat
let some = function None -> empty | Some x -> x

let i = tag "i"
let p = tag "p"
let tt = tag "tt"
let aside = tag "aside"

let nil = empty
let concat = list

let li ?cls ?id ?attrs x =
  tag ?cls ?id ?attrs "li" x

let dt ?cls ?id ?attrs x =
  tag ?cls ?id ?attrs "dt" x

let dd ?cls ?id ?attrs x =
  tag ?cls ?id ?attrs "dd" x

let ul ?(add_li=true) ?cls ?id ?attrs ?licls ls =
  let ls = if add_li then List.map (fun x -> li ?cls:licls x) ls else ls in
  tag ?cls ?id ?attrs "ul" (list ls)

let ol ?(add_li=false) ?cls ?id ?attrs ?licls ls =
  let ls = if add_li then List.map (fun x -> li ?cls:licls x) ls else ls in
  tag ?cls ?id ?attrs "ol" (list ls)

let dl ?(add_dtdd=true) ?cls ?id ?attrs ?dtcls ?ddcls lss =
  let lss = if add_dtdd
            then List.map
                   (fun (t, d) -> list [dt ?cls:dtcls t; dd ?cls:ddcls d]) lss
            else List.map (fun (t, d) -> list [t; d]) lss in
  tag ?cls ?id ?attrs "dl" (list lss)

let h1 = tag "h1"
let h2 = tag "h2"
let h3 = tag "h3"
let h4 = tag "h4"
let h5 = tag "h5"
let h6 = tag "h6"

let small = tag "small"

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

let string_of_rel = function
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
  | `tag        -> "tag"

type target =
  [ `blank
  | `parent
  | `self
  | `top
  | `Frame of string ]

let string_of_target = function
  | `blank  -> "_blank"
  | `parent -> "_parent"
  | `self   -> "_self"
  | `top    -> "_top"
  | `Frame n -> n

let a ?hreflang ?rel ?target ?ty ?title ?cls ~href html =
  let attrs = [(("", "href"), Uri.to_string href)] in
  let attrs = match hreflang with
    | Some h -> (("", "hreflang"), h) :: attrs
    | None -> attrs
  in
  let attrs = match rel with
    | Some rel ->  (("", "rel"), string_of_rel rel) :: attrs
    | None -> attrs
  in
  let attrs = match target with
    | Some t -> (("", "target"), string_of_target t) :: attrs
    | None -> attrs
  in
  let attrs = match ty with
    | Some t -> (("", "type"), t) :: attrs
    | None -> attrs
  in
  let attrs = match title with
    | Some t -> (("", "title"), t) :: attrs
    | None -> attrs
  in
  let attrs = match cls with
    | Some c -> (("", "class"), c) :: attrs
    | None -> attrs
  in
  [`El((("", "a"), attrs), html)]

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
  | None -> [`El((("", "img"), attrs), [])]

let anchor name = tag "a" ~attrs:["name", name] empty

(* color tweaks for lists *)
let interleave classes l =
  let i = ref 0 in
  let n = Array.length classes in
  let get () =
    let res = classes.(!i mod n) in
    incr i;
    res in
  List.map (Xml.tag "div" ~attrs:["class", get ()]) l

let html_of_string s = Xml.string s
let string = html_of_string

let html_of_int i = Xml.int i
let int = html_of_int

let html_of_float f = Xml.float f
let float = html_of_float

type table = t array array

let html_of_table ?(headings=false) t =
  let hd =
    if Array.length t > 0 && headings then
      let l = Array.to_list t.(0) in
      Some (tr (list @@ List.map (fun x -> th x) l))
    else
      None in
  let tl =
    if Array.length t > 1 && headings then
      List.map Array.to_list (List.tl (Array.to_list t))
    else
      List.map Array.to_list (Array.to_list t) in
  let tl = List.map (fun l -> tr (list @@ List.map (fun x -> td x) l)) tl in
  Xml.(tag "table" (some hd ++ list tl))

let append (_to : t) (el : t) = _to @ el
let (++) = append

module Create = struct
  module Tags = struct
    type html_list = [ `Ol of t list | `Ul of t list ]

    type color =
      | Rgba of char * char * char * char
      | Rgb of char * char * char

  let color_of_string ?(fmt = `Hex) s =
    let s = String.lowercase s in
    let coi = char_of_int in
    let rval = match fmt with
      | `Hex ->
        let fmt' = format_of_string "#%x" in
        let x = Scanf.sscanf s fmt' (fun x -> x) in
        let r,g,b = (x land 0xff0000) lsr 16, (x land 0xff00) lsr 8,
                    x land 0xff in
        Rgb(coi r, coi g, coi b)
      | `Rgb ->
        let fmt' = format_of_string "rgb(%d,%d,%d)" in
        let r,g,b = Scanf.sscanf s fmt' (fun a b c -> a,b,c) in
        Rgb(coi r, coi g, coi b)
    in rval

  type table_flags =
        Headings_fst_col
      | Headings_fst_row
      | Sideways
      | Heading_color of color
      | Bg_color of color

    type 'a table =
      [ `Tr of 'a table list | `Td of 'a * int * int | `Th of 'a * int * int ]
  end

  open Tags

  type t = Xml.t

  let stylesheet css =
    Xml.tag "style" ~attrs:["type","text/css"] (string css)

  let table ?(flags = [Headings_fst_row]) =
    let h_fst_col = ref false in
    let h_fst_row = ref false in
    let hdg_c = ref (color_of_string "#eDeDeD") in
    let bg_c = ref (color_of_string "#fFfFfF") in
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
    let aux ~row tbl =
      let rows = List.map row tbl in
      let rows =
        if !side then
          List.mapi (fun i _ -> List.map (fun el -> List.nth el i) rows) @@ List.hd rows
        else
          rows in
      let cellify rows = List.map (fun r -> List.map (fun x -> td x) r) rows in
      let rows =
        match !h_fst_row,!h_fst_col with
        | false,false ->
            cellify rows
        | true,false ->
            let hrow = List.hd rows |> List.map (fun x -> th x) in
            let rest = cellify (List.tl rows) in
            hrow :: rest
        | false,true ->
            List.map (fun r ->
              let h = List.hd r in
              let rest = List.map (fun x -> td x) (List.tl r) in
              th h :: rest
            ) rows
        | true,true ->
            let hrow = List.hd rows |> List.map (fun x -> th x) in
            let rest =
              List.tl rows
              |> List.map (fun r ->
                  let hcell = List.hd r in
                  let rest = List.flatten @@ cellify [List.tl r] in
                  th hcell:: rest)
            in hrow :: rest
      in
      let rows = List.map (fun r -> let r = List.flatten r in tr r) rows in
      let rows = concat rows in
      Xml.tag "table"rows
    in aux

end

let script ?src ?typ ?charset body =
  let attrs = match src with
    | None   -> []
    | Some s -> ["src",s]
  in
  let attrs = match typ with
    | None   -> attrs
    | Some t -> ("type", t) :: attrs
  in
  let attrs = match charset with
    | None   -> attrs
    | Some c -> ("charset", c) :: attrs
  in
  tag "script" ~attrs body
