
type 'a or_error = ('a, exn) result

module In : sig
  type t

  val of_unix_fd : ?bufsize:int -> Unix.file_descr -> t
  (** Create an in channel from a raw Unix file descriptor. *)

  (* NOTE: in practice, we'd also have a constructor from a C [in_channel],
     provided external for [consume] and [fill_buf] be written.

     For simplicity, this is made from scratch.
  *)

  val of_funs :
    consume:(int -> unit) ->
    fill_buf:(unit -> bytes * int * int) ->
    close:(unit -> unit) ->
    ?seek_and_pos:((int -> unit) * (unit -> int)) ->
    unit -> t

  val fill_buf : t -> (bytes * int * int)
  (** Obtain a slice of the current buffer.

      Returns [(buf, i, len)] which represents a slice of [buf]
      starting at offset [i] and of length [len].
      [len] is 0 IFF the end of input was reached. *)

  val consume : t -> int -> unit
  (** Consume n bytes from the input. *)

  val close : t -> unit
  (** Close channel and release resources. *)

  val seek : t -> int -> unit or_error
  (** If available, seek in the underlying channel. *)

  val pos : t -> int or_error
  (** If available, return current offset in underlying channel. *)
end

module Out : sig
  type t

  val of_unix_fd : ?bufsize:int -> Unix.file_descr -> t
  (** Output channel directly writing into the given Unix file descriptor. *)

  val of_funs :
    write_char:(char -> unit) ->
    write:(bytes -> int -> int -> unit) ->
    flush:(unit -> unit) ->
    close:(unit -> unit) ->
    unit -> t

  val write_char : t -> char -> unit
  val write : t -> bytes -> int -> int -> unit
  val close : t -> unit
  val flush : t -> unit

  val write_string : t -> string -> unit

  val write_bytes : t -> bytes -> unit

  val write_lines : t -> string Seq.t -> unit

  val write_int : t -> int -> unit

  (* etc. *)
end
