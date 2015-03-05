open Omd
open Cow

open OUnit

let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
let a = <:xml<<a>a</a>&>>
let b = <:xml<<b/>&>>
let c = <:xml<<c>$a$ $b$ $a$</c>&>>
let d = <:xml<<a foo=""></a>&>>
let _d = Xml.to_string d
let opt = Some a
let opt' = None
let int = 3
let flo = 5.
let str = "str"
let uri = Uri.of_string "https://github.com/mirage/ocaml-cow?q1=a&q2=b"
let alist = ["hello","world"; "class","a b"]
let list = [a; b; c]
let attrs = <:xml<hello="world" class="ab">>
let attrs' = <:xml<hello="world"  class="ab">>
let attrs'' = <:xml<hello="world" class="a b">>

let xml_expanders = [
  true, "opt",  <:xml<<z>$opt:opt$</z>&>>, "<z><a>a</a></z>";
  true, "opt'", <:xml<<z>$opt:opt'$</z>&>>, "<z/>";
  false, "int",  <:xml<$int:int$>>, string_of_int int;
  false, "flo",  <:xml<$flo:flo$>>, string_of_float flo;
  false, "str",  <:xml<$str:str$>>, str;
  false, "uri",  <:xml<$uri:uri$>>, "https://github.com/mirage/ocaml-cow?q1=a&amp;q2=b";
  true, "alist", <:xml<<tag $alist:alist$/>&>>, "<tag hello=\"world\" class=\"a b\"/>";
  true, "list", <:xml<$list:list$>>, "<a>a</a><b/><c><a>a</a> <b/> <a>a</a></c>";
  true, "attrs", <:xml<<tag $attrs:attrs$/>&>>, "<tag hello=\"world\" class=\"ab\"/>";
  true, "attrs'", <:xml<<tag $attrs:attrs'$/>&>>, "<tag hello=\"world\" class=\"ab\"/>";
  true, "attrs''", <:xml<<tag $attrs:attrs''$/>&>>, "<tag hello=\"world\" class=\"a b\"/>";
]

let suite name decl prefix =
  let suite = List.fold_left (fun acc (v, n, x, s) ->
      let aux t xml =
        let name = Printf.sprintf "%s.%s-%s" name n t in
        let str = Xml.to_string ~decl xml in
        let error =
          Printf.sprintf
            "\n\n\
             %s\n\
             is not equal to\n\
             %s\n\
             ----------------"
            str (prefix^s) in
        let test () = error @? (str = prefix^s) in
        name >:: test in
      if v then
        (aux "1" x) :: (aux "2" (Xml.of_string (Xml.to_string ~decl x))) :: acc
      else
        (aux "1" x) :: acc
    ) [] xml_expanders in
  List.rev suite

let suite =
  suite "xml_expander+dtd" true  xml_decl @
  suite "xml_expander"     false ""
