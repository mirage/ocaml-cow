include Xmlm
type t = (('a frag as 'a) frag) list

(* XXX: add a proper output_subtree function*)
let id x = x

let rec output_t o = function
  | (`Data _ as d) :: t ->
    output o d;
    output_t o t
  | (`El _ as e) :: t ->
    output_tree id o e;
    output o (`Dtd None);
    output_t o t
  | [] -> ()

let to_string ?decl t =
  let buf = Buffer.create 1024 in
  let o = make_output ?decl (`Buffer buf) in
  output o (`Dtd (Some ""));
  output_t o t;
  Buffer.contents buf

(* XXX: do a proper input_subtree integration *)
(*** XHTML parsing (using Xml) ***)
let _input_tree input : t =
  let el (name, attrs) body : t = [ `El ((name, attrs), List.concat body) ] in
  let data str : t = [`Data str] in
  input_tree ~el ~data input

let of_string ?entity ?enc str =
  (* It is illegal to write <:html<<b>foo</b>>> so we use a small trick and write
<:html<<b>foo</b>&>> *)
  let str = if str.[String.length str - 1] = '&' then
    String.sub str 0 (String.length str - 1)
  else
    str in
  (* input needs a root tag *)
  let str = Printf.sprintf "<xxx>%s</xxx>" str in
  try
    let i = make_input ~enc ?entity (`String (0,str)) in
    (* make_input builds a well-formed document, so discard the Dtd *)
    (match peek i with
      | `Dtd _ -> let _ = input i in ()
      | _ -> ());
    (* Remove the dummy root tag *)
    match _input_tree i with
      | [ `El ((("","xxx"), []), body) ]-> body
      | _ -> raise Parsing.Parse_error
  with Error (pos, e) ->
    Printf.eprintf "[XMLM:%d-%d] %s: %s\n"(fst pos) (snd pos) str (error_message e);
    raise Parsing.Parse_error
