open Cow

let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
let a = Xml.(tag "a" (string "a"))
let b = Xml.(tag "b" empty)
let c = Xml.(tag "c" (a ++ string " " ++ b ++ string " " ++ a))
let d = Xml.(tag "a" ~attrs:["foo",""] empty)
let _d = Xml.to_string d
let opt = Some a
let opt' = None
let int = 3
let flo = 5.
let str = "str"
let uri = Uri.of_string "https://github.com/mirage/ocaml-cow?q1=a&q2=b"
let alist = ["hello","world"; "class","a b"]
let list = [a; b; c]
let attrs = ["hello","world"; "class","ab"]
let attrs' = ["hello","world"; "class","ab"]
let attrs'' = ["hello","world"; "class","a b"]

let make_xml = Xml.(tag "tag" empty)
let make_xml_result = "<tag/>"

let xml_expanders = [
  true, "opt",  Xml.(tag "z" (some opt)), "<z><a>a</a></z>";
  true, "opt'", Xml.(tag "z" (some opt')), "<z/>";
  false, "int",  Xml.int int, string_of_int int;
  false, "flo",  Xml.float flo, string_of_float flo;
  false, "str",  Xml.string str, str;
  false, "uri",  Xml.uri uri, "https://github.com/mirage/ocaml-cow?q1=a&amp;q2=b";
  true, "alist", Xml.(tag "tag" ~attrs:alist empty), "<tag hello=\"world\" class=\"a b\"/>";
  true, "list", Xml.list list, "<a>a</a><b/><c><a>a</a> <b/> <a>a</a></c>";
  true, "attrs", Xml.(tag "tag" ~attrs:attrs empty), "<tag hello=\"world\" class=\"ab\"/>";
  true, "attrs'", Xml.(tag "tag" ~attrs:attrs' empty), "<tag hello=\"world\" class=\"ab\"/>";
  true, "attrs''", Xml.(tag "tag" ~attrs:attrs'' empty), "<tag hello=\"world\" class=\"a b\"/>";
  true, "labeled_xml_fn", make_xml, make_xml_result;
  true, "label_xml_fn_assignment", make_xml, make_xml_result;
]

let suite name decl prefix: Alcotest.test_case list =
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
        let test () = Alcotest.(check string) error str (prefix^s) in
        name, `Quick, test
      in
      if v then
        (aux "1" x) :: (aux "2" (Xml.of_string (Xml.to_string ~decl x))) :: acc
      else
        (aux "1" x) :: acc
    ) [] xml_expanders
  in
  List.rev suite

let suite =
  suite "xml_expander+dtd" true  xml_decl @
  suite "xml_expander"     false ""
