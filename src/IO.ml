

type 'a or_error = ('a, exn) result

(* tiny internal buffer implementation *)
module MiniBuf = struct
  type t = {
    mutable bytes: bytes;
    mutable len: int; (* slice is [bytes[0..len]] *)
  }

  let create ?(size=4_096) () : t =
    { bytes=Bytes.create size; len=0 }

  let size self = self.len
  let bytes_slice self = self.bytes
  let clear self : unit =
    if Bytes.length self.bytes > 4_096 * 1_024 then (
      self.bytes <- Bytes.create 4096; (* really big buffer, free it *)
    );
    self.len <- 0

  let resize self new_size : unit =
    let new_buf = Bytes.create new_size in
    Bytes.blit self.bytes 0 new_buf 0 self.len;
    self.bytes <- new_buf

  let ensure self new_size : unit =
    (* need to resize? *)
    if new_size >= Bytes.length self.bytes then (
      (* amortized *)
      let new_cap =
        min Sys.max_string_length (new_size + self.len / 2 + 10)
      in
      if new_cap < new_size then failwith "maximum bytes size exceeded";
      resize self new_cap;
    )

  let add_bytes (self:t) s i len : unit =
    ensure self (self.len + len); (* resize if needed? *)
    Bytes.blit s i self.bytes self.len len;
    self.len <- self.len + len

  let contents (self:t) : string = Bytes.sub_string self.bytes 0 self.len
end

module In = struct
  type t = {
    fill_buf:unit -> bytes * int * int;
    consume:int -> unit;
    close:unit -> unit;
    seek_and_pos: ((int -> unit) * (unit -> int)) option;
  }

  let of_unix_fd ?(bufsize=4096) fd : t =
    let buf = Bytes.create bufsize in
    let i = ref 0 in
    let len = ref 0 in

    let consume n = i := !i + n in
    let fill_buf () =
      if !i >= !len then (
        i := 0;
        len := Unix.read fd buf 0 (Bytes.length buf);
      );
      buf, !i, !len - !i
    in
    let seek i = ignore (Unix.lseek fd i Unix.SEEK_SET : int) in
    let pos () = Unix.lseek fd 0 Unix.SEEK_CUR in
    let close () = Unix.close fd in
    { consume; close; fill_buf; seek_and_pos=Some((seek,pos)) }

  let of_funs ~consume ~fill_buf ~close ?seek_and_pos () : t =
    {consume; fill_buf; close; seek_and_pos}

  let[@inline] fill_buf self = self.fill_buf()
  let[@inline] close self = self.close()
  let[@inline] consume self n = self.consume n

  let[@inline] seek self n =
    match self.seek_and_pos with
    | None -> Error (Failure "seek not available")
    | Some (s,_) -> Ok (s n)

  let[@inline] pos self =
    match self.seek_and_pos with
    | None -> Error (Failure "pos not available")
    | Some (_,p) -> Ok (p ())
end

module Out = struct
  type t = {
    write_char : char -> unit;
    write: bytes -> int -> int -> unit;
    flush: unit -> unit;
    close: unit -> unit;
  }

  let of_unix_fd ?(bufsize=4096) fd : t =
    let buf = Bytes.create bufsize in
    let b_len = ref 0 in

    (* really write content *)
    let flush () =
      let i = ref 0 in
      while !i < !b_len do
        let nw = Unix.single_write fd buf !i (!b_len - !i) in
        i := !i + nw
      done;
      b_len := 0
    in
    let close() =
      flush();
      Unix.close fd
    and write_char c =
      if !b_len = bufsize then flush();
      Bytes.set buf !b_len c;
      incr b_len
    and write bs i len : unit =
      let len = ref len in
      while !len > 0 do
        if !b_len = bufsize then flush (); (* make room *)
        assert (!b_len < bufsize);

        let free_space = bufsize - !b_len in
        let nw = min free_space !len in (* how much can we write in one go? *)
        Printf.eprintf "write: b-len=%d len=%d len'=%d\n%!" !b_len !len nw;
        assert (nw>0);
        Bytes.blit bs i buf !b_len nw;
        b_len := !b_len + nw;
        len := !len - nw;
      done
    in
    {write; write_char; flush; close}

  let of_funs ~write_char ~write ~flush ~close () : t =
    { write; write_char; flush; close }

  let[@inline] write self bs i len = self.write bs i len
  let[@inline] write_char self c = self.write_char c
  let[@inline] close self = self.close ()
  let[@inline] flush self = self.flush ()

  let write_bytes self b = write self b 0 (Bytes.length b)

  let write_string self s = write_bytes self (Bytes.unsafe_of_string s)

  let write_int self i =
    let s = string_of_int i in
    write_string self s

  let write_lines self seq =
    Seq.iter
      (fun s ->
        write_string self s;
        write_char self '\n')
      seq

  (* etc. *)
end
