
open Cow

let _ =
Html.output_doc (`Channel (open_out "basic.html"))
  Html.(
    html (list [
      head (list [
        base ~href:(Uri.of_string "http://www.example.com") empty;
        meta ~charset:"UTF-8" empty;
        title (string "An application with a long head");
        link ~rel:"stylesheet" ~href:(Uri.of_string "default.css") empty;
        link ~rel:"stylesheet alternate"
             ~href:(Uri.of_string "big.css")
             ~title:"Big text"
             empty;
        script ~src:"support.js" empty;
        meta ~name:"application-name"
             ~content:"Long headed application"
             empty
      ]);

      body (list [
        h1 (string "Test page");

        p (list [
          string "This ";
          code (string "page");
          string " is for ";
          b (string "testing");
          string "."
        ]);

        h2 (string "Another heading");
        p (list [
          string "x"; sub (string "1");
          string " + ";
          string "x"; sub (string "2"); sup (string "3");
        ])
      ])
    ])
  )

