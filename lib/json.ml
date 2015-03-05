(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

include Ezjsonm

let rec list_iter_between f o = function
  | []   -> ()
  | [h]  -> f h
  | h::t -> f h; o (); list_iter_between f o t

let escape_string s =
  let buf = Buffer.create 80 in
  Buffer.add_string buf "\"";
  for i = 0 to String.length s - 1
  do
    let x =
      match s.[i] with
      | '\n'   -> "\\n"
      | '\t'   -> "\\t"
      | '\r'   -> "\\r"
      | '\b'   -> "\\b"
      | '\\'   -> "\\\\"
      | '/'    -> "\\/"
      | '"'    -> "\\\""
      | '\x0c' -> "\\f"
      | c      -> String.make 1 c
    in
    Buffer.add_string buf x
  done;
  Buffer.add_string buf "\"";
  Buffer.contents buf

let rec to_fct t f =
  match t with
  | `Bool b   -> f (string_of_bool b)
  | `Float r  -> f (Printf.sprintf "%g" r)
  | `String s -> f (escape_string s)
  | `Null     -> f "null"
  | `A a   ->
    f "[";
    list_iter_between (fun i -> to_fct i f) (fun () -> f ", ") a;
    f "]";
  | `O a   ->
    f "{";
    list_iter_between (fun (k, v) -> to_fct (`String k) f; f ": "; to_fct v f)
      (fun () -> f ", ") a;
    f "}"

let rec to_fct_hum t f =
  match t with
  | `Bool b   -> f (string_of_bool b)
  | `Float r  -> f (Printf.sprintf "%g" r)
  | `String s -> f (escape_string s)
  | `Null     -> f "null"
  | `A a ->
    f "[ ";
    list_iter_between (fun i -> to_fct i f) (fun () -> f ", ") a;
    f " ]\n";
  | `O a   ->
    f "{";
    list_iter_between (fun (k, v) -> to_fct (`String k) f; f ": "; to_fct v f)
      (fun () -> f ", ") a;
    f "}\n"


let to_buffer v buf = to_fct v (fun s -> Buffer.add_string buf s)
let to_string v = Ezjsonm.to_string ~minify:true (wrap v)
let to_buffer_hum v buf = to_fct_hum v (fun s -> Buffer.add_string buf s)
let to_string_hum v = Ezjsonm.to_string ~minify:false (wrap v)
let of_string s = unwrap (Ezjsonm.from_string s)

exception Runtime_error of string * value
