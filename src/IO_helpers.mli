
(** Functions purely in userland *)

open IO

val in_of_string : ?off:int -> ?len:int -> string -> In.t
(** An input channel reading from the string *)

val in_of_bytes : ?off:int -> ?len:int -> bytes -> In.t
(** An input channel reading from the bytes buffer *)

type filename = string

val with_in : filename -> (In.t -> 'a) -> 'a
val with_out : filename -> (Out.t -> 'a) -> 'a

val with_in_l : filename list -> (In.t list -> 'a) -> 'a

val out_of_buffer : Buffer.t -> Out.t

val concat : In.t list -> In.t
(** Read from each stream, in order *)

val read_all : In.t -> string
(** Input all the content into a string *)

val read_line: In.t -> string option

val read_lines: In.t -> string list

val map_c : ?buf_size:int -> (char -> char) -> In.t -> In.t

val rot13 : ?buf_size:int -> In.t -> In.t
(** An example of {!map_c} *)

val read_at_most : close_rec:bool -> int -> In.t -> In.t
(** [read_at_most n ic] behaves like [ic] but stops after reading at
    most [n] bytes. Does not consume more than [n] bytes.
    @param close_rec if true, when closed, the inner channel [ic] is also closed
    *)

val read_all_into : In.t -> Buffer.t -> unit
(** Input all the content into the buffer *)

val copy : In.t -> Out.t -> unit

module Gzip : sig
  val decode : ?buf_size:int -> In.t -> In.t

  val encode : ?buf_size:int -> In.t -> In.t
end
