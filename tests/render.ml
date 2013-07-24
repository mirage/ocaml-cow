open Cow

open OUnit

let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
let a = <:xml<<a>a</a>&>>
let b = <:xml<<b/>&>>
let c = <:xml<<c>$a$ $b$ $a$</c>&>>
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
  "opt",  <:xml<<z>$opt:opt$</z>&>>, "<z><a>a</a></z>";
  "opt'", <:xml<<z>$opt:opt'$</z>&>>, "<z/>";
  "int",  <:xml<$int:int$>>, string_of_int int;
  "flo",  <:xml<$flo:flo$>>, string_of_float flo;
  "str",  <:xml<$str:str$>>, str;
  "uri",  <:xml<$uri:uri$>>, "https://github.com/mirage/ocaml-cow?q1=a&amp;q2=b";
  "alist", <:xml<<tag $alist:alist$/>&>>, "<tag hello=\"world\" class=\"a b\"/>";
  "list", <:xml<$list:list$>>, "<a>a</a><b/><c><a>a</a> <b/> <a>a</a></c>";
  "attrs", <:xml<<tag $attrs:attrs$/>&>>, "<tag hello=\"world\" class=\"ab\"/>";
  "attrs'", <:xml<<tag $attrs:attrs'$/>&>>, "<tag hello=\"world\" class=\"ab\"/>";
  "attrs''", <:xml<<tag $attrs:attrs''$/>&>>, "<tag hello=\"world\" class=\"a b\"/>";
]

let suite name decl prefix =
  List.map (fun (n, x, s) ->
      let name = Printf.sprintf "%s-%s" name n in
      let xml_str = Xml.to_string ~decl x in
      let error =
        Printf.sprintf
          "\n\n\
           %s\n\
           is not equal to\n\
           %s\n\
          ----------------"
          xml_str (prefix^s) in
      let test () =
        error @? (xml_str = prefix^s) in
      name >:: test
    ) xml_expanders

let _ =
  let with_dtd = suite "xml_expander+dtd" true xml_decl in
  let without_dtd  = suite "xml_expander" false "" in
  run_test_tt_main
    ("COW" >::: (with_dtd @ without_dtd))
