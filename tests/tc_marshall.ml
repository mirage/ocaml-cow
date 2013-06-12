open Cow

type broken = {
  foo: string option;
  bar: string;
} with xml,json

type p =
  | One of string * int array * char * bool * (float list)
  | Two of t
  | Three of x option list

and pp = [ `Poly1 | `Poly2 | `Poly3 of int ]

and t = {
  t1: int;
  mutable t2: string;
  t3: x
} and x = {
  x1: t array;
  x2: int64
} and f = {
  mutable f1: int;
  mutable f2: string list;
  f3: string;
  f4: int64;
  f5: char array;
} and tu = ( int  * f * pp )
with xml,json,html

type o = < x: f; y: string; z: int option >
with xml,json,html

open OUnit

let _ = Random.self_init ()

let char () = Char.chr (Random.int 25 + 97)
let int () = Random.int 10000000
let int64 () = Random.int64 Int64.max_int
let option v = if Random.int 4 = 0 then None else Some (v ())
let float () = Random.float 1000000.
let bool () = Random.int 1 = 0

let string () =
  let len = Random.int 30 in
  let s = String.create len in
  for i = 0 to len - 1 do
    String.set s i (char ())
  done;
  s

let array v =
  let len = Random.int 30 in
  let s = Array.create len (v()) in
  for i = 0 to len - 1 do
    s.(i) <- v ()
  done;
  s

let list v = Array.to_list (array v)

let rec x1 = { x1 = [| t1; t2 |]; x2 = int64 () }
and t1 = { t1 = int (); t2 = string (); t3 = x1 }
and t2 = { t1 = int (); t2 = string (); t3 = x1 }

let rec p () =
  match Random.int 3 with
  | 0 -> One (string (), array int, char (), bool (), list float)
  | 1 -> Two (t ())
  | 2 -> Three (list (fun () -> option x))
  | _ -> assert false

and pp () =
  match Random.int 3 with
  | 0 -> `Poly1
  | 1 -> `Poly2
  | 2 -> `Poly3 (int ())
  | _ -> assert false

and t () = if Random.int 10 > 1 then t1 else { t1 = int (); t2 = string (); t3 = x () }

and x () = if Random.int 10 > 1 then x1 else { x1 = array t; x2 = int64 () }

and f () = { f1 = int (); f2 = list string; f3 = string (); f4 = int64 (); f5 = array char }

and tu ()  = ( int (), f (), pp ())

let o () : o = object method x = f () method y = string () method z = option int end

let check_xml n f x =
  let v1 = f x in
  Printf.printf "%s(v1): %s\n%!" n (Xml.to_string v1)

let check_json n f g x =
  let v1 = f x in
  let v2 = f (g v1) in
  if not (v1 = v2) then begin
    Printf.printf "%s(v1): %s\n%!" n (Json.to_string v1);
    Printf.printf "%s(v2): %s\n%!" n (Json.to_string v2);
  end;
  ("json.EQ " ^ n) @? (v1 = v2)

let check n (f1) (f2,g2) x =
  check_xml n f1 x;
  check_json n f2 g2 x

let test_tuple_marshal () =
  for i = 1 to 200 do begin
    let f = f () in
    let tu = tu () in
    check "f" (xml_of_f) (json_of_f, f_of_json) f;
    check "tu" (xml_of_tu) (json_of_tu, tu_of_json) tu;
  end done

let test_rec_marshal () =
  for i = 1 to 200 do begin
    let t = t () in
    let x = x () in
    check "t" (xml_of_t) (json_of_t, t_of_json) t;
    check "x" (xml_of_x) (json_of_x, x_of_json) x;
  end done

let test_variant_marshal () =
  for i = 1 to 200 do begin
    let p = p () in
    check "p" (xml_of_p) (json_of_p, p_of_json) p;
  end done

let test_polyvar_marshal () =
  for i = 1 to 200 do begin
    let pp = pp () in
    check "pp" (xml_of_pp) (json_of_pp, pp_of_json) pp;
  end done

let test_object_marshal () =
  for i = 1 to 200 do begin
    let o = o () in
    check "o" (xml_of_o) (json_of_o, o_of_json) o;
  end done

let suite = [
  "variant_marshal" >:: test_variant_marshal;
  "polyvar_marshal" >:: test_polyvar_marshal;
  "tuple_marshal"   >:: test_tuple_marshal;
  "rec_marshal"     >:: test_rec_marshal;
  "object_marshal"  >:: test_object_marshal;
]

let _ =
  run_test_tt_main ("COW" >::: suite)
