open Cow

let _ =
  Html.output_doc
    (`Channel (open_out "basic.html"))
    Html.(
      html
        (list
           [
             head
               (list
                  [
                    base (Uri.of_string "http://www.example.com");
                    meta ~charset:"UTF-8" [];
                    title (string "An application with a long head");
                    link ~rel:"stylesheet" (Uri.of_string "default.css");
                    link ~rel:"stylesheet alternate" ~title:"Big text"
                      (Uri.of_string "big.css");
                    script ~src:(Uri.of_string "support.js") empty;
                    meta ~name:"application-name"
                      ~content:"Long headed application" [];
                  ]);
             body
               (list
                  [
                    h1 (string "Test page");
                    p
                      (list
                         [
                           string "This ";
                           code (string "page");
                           string " is for ";
                           b (string "testing");
                           string ".";
                         ]);
                    h2 (string "Another heading");
                    p
                      (list
                         [
                           string "x";
                           sub (string "1");
                           string " + ";
                           string "x";
                           sub (string "2");
                           sup (string "3");
                         ]);
                  ]);
           ]))
