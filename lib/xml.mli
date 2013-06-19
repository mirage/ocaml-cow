include module type of Xmlm
type t = (('a frag as 'a) frag) list
val to_string : t -> string
val of_string :
  ?entity:(string -> string option) ->
  ?enc:encoding ->
  string -> t
