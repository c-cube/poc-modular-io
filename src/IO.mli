module In : sig
  type t = private {
    input: bytes -> int -> int -> int;
        (** Read into the slice. Returns [0] only if the
        channel is closed. *)
    close: unit -> unit;  (** Close the input. Must be idempotent. *)
    as_fd: unit -> Unix.file_descr option;
  }
  (** An input channel, i.e an incoming stream of bytes.

      This can be a [string], an [int_channel], an [Unix.file_descr], a
      decompression wrapper around another input channel, etc. *)

  val create :
    ?as_fd:(unit -> Unix.file_descr option) ->
    ?close:(unit -> unit) ->
    input:(bytes -> int -> int -> int) ->
    unit ->
    t

  val of_unix_fd : ?close_noerr:bool -> Unix.file_descr -> t
  (** Create an in channel from a raw Unix file descriptor. *)

  val of_in_channel : ?close_noerr:bool -> in_channel -> t
  val input : t -> bytes -> int -> int -> int
  val input_into_buf : t -> Buf.t -> unit

  val seek : t -> int64 -> unit
  (** If available, seek in the underlying channel.
      @raise Sys_error in case of failure *)

  val pos : t -> int64
  (** If available, return current offset in underlying channel.
      @raise Sys_error in case of failure *)

  val close : t -> unit
end

(** Buffered channel *)
module In_buffered : sig
  type t = private {
    mutable buf: Buf.t;
    mutable off: int;
    fill_buffer: t -> int;
        (** Refill [buf], return new offset, modifies size of [buf].
            Precondition: [Buf.size buf=0] *)
    close: unit -> unit;
  }

  val create :
    ?buf:Buf.t -> ?close:(unit -> unit) -> fill_buffer:(t -> int) -> unit -> t

  val fill_buffer : t -> unit
  (** [fill_buffer bic] ensures that [bic.buf] is empty only if [bic.ic]
      is empty. Always call this before looking at [bic.buf].*)

  val fill_and_get : t -> bytes * int * int
  val get_bytes : t -> bytes
  val get_off : t -> int
  val get_len : t -> int
  val input : t -> bytes -> int -> int -> int
  val of_in : ?buf:Buf.t -> In.t -> t

  val consume : t -> int -> unit
  (** [consume bic n] consumes [n] bytes from [bic].
      Precondition: [n <= get_len bic] *)

  val close : t -> unit
  val into_in : t -> In.t
end

module Out : sig
  type t = private {
    output_char: char -> unit;  (** Output a single char *)
    output: bytes -> int -> int -> unit;  (** Output slice *)
    flush: unit -> unit;  (** Flush underlying buffer *)
    close: unit -> unit;  (** Close the output. Must be idempotent. *)
    as_fd: unit -> Unix.file_descr option;
  }
  (** An output channel, ie. a place into which we can write bytes.

      This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc. *)

  val create :
    ?as_fd:(unit -> Unix.file_descr option) ->
    ?flush:(unit -> unit) ->
    ?close:(unit -> unit) ->
    output_char:(char -> unit) ->
    output:(bytes -> int -> int -> unit) ->
    unit ->
    t

  val of_out_channel : ?close_noerr:bool -> out_channel -> t

  val of_unix_fd : Unix.file_descr -> t
  (** Output channel directly writing into the given Unix file descriptor. *)

  val output_char : t -> char -> unit
  val output_buf : t -> Buf.t -> unit
  val output : t -> bytes -> int -> int -> unit
  val close : t -> unit
  val flush : t -> unit
  val output_string : t -> string -> unit
  val output_lines : t -> string Seq.t -> unit
  val output_int : t -> int -> unit
  val pos : t -> int64
  val seek : t -> int64 -> unit
end
