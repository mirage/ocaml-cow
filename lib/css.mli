(*
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

(** Single element *)
type elt =
  | Str of string
  | Fun of string * expr list

(** Expression: `.body a:hover`. No commas here. *)
and expr = elt list

(** We allow nested declarations *)
type prop_decl =
  (** Property: {v `background-color: blue, red;` v} *)
  | Prop of string * expr list
  | Decl of expr list * prop_decl list

(** Utility type used to specify the type of gradient to be
    emitted by [polygradient] *)
type gradient_type = [ `Linear | `Radial ]

(** The type of CSS fragment *)
type t =
  | Props of prop_decl list
  | Exprs of expr list

(** Print CSS to a [string] suitable for rendering *)
val to_string : t -> string

(** emits CSS containing the contents of the argument, suitable for
    embedding in CSS format strings *)
val of_string : string -> t

(** {2 Getters} *)

val expr : t -> expr
val exprs : t -> expr list
val props : t -> prop_decl list
val string : t -> string

(** {3 Helpers} *)

(** transform a fragment with nested declarations into
    an equivalent fragment with only root declarations *)
val unroll : t -> t

(** {2 CSS library} *)

(** Emit a CSS gradient style that can be either a linear
    gradient or a radial gradient (at the moment it will
    by default produce a centered circular gradient if
    asked for a radial gradient). For example, a call of
    [polygradient `Radial <:css<circle>> <:css<#ff0000>> <:css<#00ff00>>]
    will produce a red and green centered circular radial
    gradient. *)
val polygradient
  : gradient_type ->
    ?behaviour:t ->
    ?low:t ->
    ?high:t ->
    unit -> t

(** Emit a CSS gradient style that linearly interpolates
    between the [low] and [high] colors moving from top to
    bottom; the default values of [low] and [high] are
    [#0a0a0a] and [#ffffff] respectively *)
val gradient
  : ?low:t ->
    ?high:t ->
    unit -> t

(** Emit a border style that rounds off the top border by
    [0.5em] pixels. *)
val top_rounded : ?radius:t -> unit -> t

(** Emit a border style that rounds off the bottom border
    by [0.5em] pixels. *)
val bottom_rounded : ?radius:t -> unit -> t

(** Emit a border style that rounds off all the borders by
    [radius], which has a default value of [0.5em] *)
val rounded : ?radius:t -> unit -> t

val box_shadow : ?h:t -> ?v:t -> ?blur:t -> ?color:t -> unit -> t
val text_shadow : ?h:t -> ?v:t -> ?blur:t -> ?color:t -> unit -> t

val no_padding : t
val reset_padding : t
