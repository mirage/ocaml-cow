open Cow

open OUnit

let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n"
let a = <:xml<<a>a</a>&>>
let b = <:xml<<b/>&>>
let c = <:xml<<c>$a$ $b$ $a$</c>&>>
let opt = Some a
let opt' = None
let int = 3
let flo = 5.
let str = "str"
let uri = Uri.of_string "https://github.com/mirage/ocaml-cow"
(*let alist = ["hello","world"; "key","value"] in*)
let list = [a; b; c]
(*let attrs = *)
let xml_expanders = [
  "opt",  <:xml<<z>$opt:opt$</z>&>>, "<z><a>a</a></z>";
  "opt'", <:xml<<z>$opt:opt'$</z>&>>, "<z/>";
  "int",  <:xml<$int:int$>>, string_of_int int;
  "flo",  <:xml<$flo:flo$>>, string_of_float flo;
  "str",  <:xml<$str:str$>>, str;
  "uri",  <:xml<$uri:uri$>>, Uri.to_string uri;
  (*"alist", <:xml< $alist:alist$ >>, "";*)
  "list", <:xml<$list:list$>>, "<a>a</a><b/><c><a>a</a> <b/> <a>a</a></c>";
  (*"attrs", <:xml< <tag $attrs:attrs$></tag> >>, "";*)
]

let suite =
  (List.map (fun (n, x, s) ->
    let xml_str = Xml.to_string x in
    ("xml_expanders."^n) >:: (fun () ->
      ((xml_str^"\nis not equal to\n"^xml_decl^s) @? (xml_str = xml_decl^s))
     )) xml_expanders)

let _ =
  run_test_tt_main ("COW" >::: suite)
