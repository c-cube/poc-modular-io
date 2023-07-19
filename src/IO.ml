module In = struct
  type t = {
    input: bytes -> int -> int -> int;
    close: unit -> unit;
    as_fd: unit -> Unix.file_descr option;
  }

  let create ?(as_fd = fun _ -> None) ?(close = ignore) ~input () : t =
    { as_fd; close; input }

  let of_in_channel ?(close_noerr = false) (ic : in_channel) : t =
    {
      input = (fun buf i len -> input ic buf i len);
      close =
        (fun () ->
          if close_noerr then
            close_in_noerr ic
          else
            close_in ic);
      as_fd = (fun () -> Some (Unix.descr_of_in_channel ic));
    }

  let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t =
    {
      input = (fun buf i len -> Unix.read fd buf i len);
      as_fd = (fun () -> Some fd);
      close =
        (fun () ->
          if close_noerr then (
            try Unix.close fd with _ -> ()
          ) else
            Unix.close fd);
    }

  (** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
  let[@inline] input (self : t) buf i len = self.input buf i len

  let input_into_buf (self : t) (buf : Buf.t) : unit =
    let n = self.input buf.bytes 0 (Bytes.length buf.bytes) in
    buf.len <- n

  (** Close the channel. *)
  let[@inline] close self : unit = self.close ()

  let seek self i : unit =
    match self.as_fd () with
    | Some fd -> ignore (Unix.lseek fd (Int64.to_int i) Unix.SEEK_SET : int)
    | None -> raise (Sys_error "cannot seek")

  (* TODO: find how to do that from a FD *)
  let pos _self : int64 = raise (Sys_error "cannot get pos")
end

(** Buffered channel *)
module In_buffered = struct
  type t = {
    mutable buf: Buf.t;
    mutable off: int;
    fill_buffer: t -> int;
    close: unit -> unit;
  }

  let[@inline] get_bytes self = self.buf.bytes
  let[@inline] get_off self = self.off
  let[@inline] get_len self = Buf.size self.buf - self.off

  (** Make sure [self.buf] is not empty if [ic] is not empty *)
  let fill_buffer (self : t) : unit =
    if get_len self <= 0 then (
      Buf.clear self.buf;
      self.off <- self.fill_buffer self
    )

  let create ?(buf = Buf.create ()) ?(close = ignore) ~fill_buffer () : t =
    { buf; off = 0; close; fill_buffer }

  let input self b i len : int =
    fill_buffer self;
    let n_available = Buf.size self.buf - self.off in
    if n_available > 0 then (
      let n = min len n_available in
      Bytes.blit self.buf.bytes self.off b i n;
      self.off <- self.off + n;
      n
    ) else
      0

  let[@inline] fill_and_get self =
    fill_buffer self;
    get_bytes self, get_off self, get_len self

  let consume self n =
    assert (n <= get_len self);
    self.off <- self.off + n

  let close self = self.close ()

  let of_in ?buf ic : t =
    let close () = In.close ic in
    let fill_buffer self : int =
      assert (Buf.size self.buf = 0);
      In.input_into_buf ic self.buf;
      0
    in
    create ?buf ~close ~fill_buffer ()

  let into_in (self : t) : In.t =
    let input b i len = input self b i len in
    let close () = close self in
    In.create ~close ~input ()
end

module Out = struct
  type t = {
    output_char: char -> unit;  (** Output a single char *)
    output: bytes -> int -> int -> unit;  (** Output slice *)
    flush: unit -> unit;  (** Flush underlying buffer *)
    close: unit -> unit;  (** Close the output. Must be idempotent. *)
    as_fd: unit -> Unix.file_descr option;
  }

  let create ?(as_fd = fun () -> None) ?(flush = ignore) ?(close = ignore)
      ~output_char ~output () : t =
    { as_fd; flush; close; output_char; output }

  (** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
  let of_out_channel ?(close_noerr = false) (oc : out_channel) : t =
    {
      output_char = (fun c -> output_char oc c);
      output = (fun buf i len -> output oc buf i len);
      flush = (fun () -> flush oc);
      close =
        (fun () ->
          if close_noerr then
            close_out_noerr oc
          else
            close_out oc);
      as_fd = (fun () -> Some (Unix.descr_of_out_channel oc));
    }

  let of_unix_fd fd : t = of_out_channel (Unix.out_channel_of_descr fd)

  (** [of_buffer buf] is an output channel that writes directly into [buf].
        [flush] and [close] have no effect. *)
  let of_buffer (buf : Buffer.t) : t =
    {
      output_char = Buffer.add_char buf;
      output = Buffer.add_subbytes buf;
      flush = ignore;
      close = ignore;
      as_fd = (fun () -> None);
    }

  (** Output the buffer slice into this channel *)
  let[@inline] output_char (self : t) c : unit = self.output_char c

  (** Output the buffer slice into this channel *)
  let[@inline] output (self : t) buf i len : unit = self.output buf i len

  let[@inline] output_string (self : t) (str : string) : unit =
    self.output (Bytes.unsafe_of_string str) 0 (String.length str)

  (** Close the channel. *)
  let[@inline] close self : unit = self.close ()

  (** Flush (ie. force write) any buffered bytes. *)
  let[@inline] flush self : unit = self.flush ()

  let seek self i : unit =
    match self.as_fd () with
    | Some fd -> ignore (Unix.lseek fd (Int64.to_int i) Unix.SEEK_SET : int)
    | None -> raise (Sys_error "cannot seek")

  (* TODO: do it from fd *)
  let pos _self : int64 = raise (Sys_error "cannot get pos")

  let output_buf (self : t) (buf : Buf.t) : unit =
    let b = Buf.bytes_slice buf in
    output self b 0 (Buf.size buf)

  let output_int self i =
    let s = string_of_int i in
    output_string self s

  let output_lines self seq =
    Seq.iter
      (fun s ->
        output_string self s;
        output_char self '\n')
      seq

  (* etc. *)
end
