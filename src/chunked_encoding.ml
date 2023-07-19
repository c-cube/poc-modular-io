open IO

let encode_out ?(chunk_size = 4_096) oc : Out.t =
  let buf = Bytes.create chunk_size in
  let len = ref 0 in
  let write_header n =
    let header = Printf.sprintf "%x\r\n" n in
    Out.output_string oc header
  in
  (* write the content of [buf] as a chunk *)
  let flush () =
    if !len > 0 then (
      write_header !len;
      Out.output oc buf 0 !len;
      len := 0;
      Out.output_string oc "\r\n"
    );
    Out.flush oc
  in
  let rec output buf2 i2 len2 =
    let n2 = min (chunk_size - !len) len2 in
    (* how much of [buf2] we can actually consume *)
    let size_chunk = !len + n2 in
    if size_chunk > 0 then (
      write_header size_chunk;
      if !len > 0 then (
        Out.output oc buf 0 !len;
        len := 0
      );
      Out.output oc buf2 i2 n2;
      Out.output_string oc "\r\n";
      (output [@tailcall]) buf2 (i2 + n2) (len2 - n2)
    )
  in
  let close () =
    flush ();
    Out.output_string oc "0\r\n\r\n";
    (* empty chunk *)
    Out.flush oc;
    Out.close oc
  and output_char c =
    if !len = chunk_size then flush ();
    assert (!len < chunk_size);
    Bytes.set buf !len c;
    incr len
  in
  Out.create ~close ~flush ~output_char ~output ()

let encode_in ?(chunk_size = 4_096) (ic : In.t) : In_buffered.t =
  (* max size for the chunk size part *)
  let offset_meta = 18 in
  let buf = Buf.create ~size:(min 32 (chunk_size + offset_meta)) () in
  let close () = In.close ic in

  let closed = ref false in

  let fill_buffer (self : In_buffered.t) : int =
    if !closed then
      0
    else (
      let len =
        In.input ic self.buf.bytes offset_meta
          (Bytes.length self.buf.bytes - offset_meta)
      in

      if len > 0 then (
        (* write prefix, return offset *)
        let header = Printf.sprintf "%x\r\n" len in
        assert (String.length header < offset_meta);
        let len_header = String.length header in
        let new_off = offset_meta - len_header in
        Bytes.blit_string header 0 self.buf.bytes new_off len_header;
        new_off
      ) else (
        (* write terminating sequence at offset 0 *)
        closed := true;
        let header = "0\r\n\r\n" in
        let len_header = String.length header in
        Bytes.blit_string header 0 self.buf.bytes 0 len_header;
        self.buf.len <- len_header;
        0
      )
    )
  in
  In_buffered.create ~buf ~close ~fill_buffer ()

let decode_in (ic : In_buffered.t) : In_buffered.t =
  let first = ref true in
  let eof = ref false in

  let read_next_chunk_len () : int =
    (* read interspersed "\r\n" if needed *)
    if !first then
      first := false
    else (
      match IO_helpers.read_line ic with
      | None -> failwith "expected crlf between chunks"
      | Some "\r" -> ()
      | Some s ->
        failwith @@ Printf.sprintf "expected crlf between chunks, not %S" s
    );
    match IO_helpers.read_line ic with
    | None -> failwith "expected <length crlf> at start of chunk"
    | Some line ->
      (* parse length, drop optional extension *)
      (try Scanf.sscanf line "%x %s@\r" (fun n _ext -> n)
       with _ -> failwith @@ "cannot read chunk size from line " ^ line)
  in
  (* how many bytes remain to be read from current chunk *)
  let n_missing = ref 0 in

  let fill_buffer (self : In_buffered.t) : int =
    if not !eof then (
      if !n_missing = 0 then (
        (* not in a chunk, read the size of the next one *)
        let len = read_next_chunk_len () in
        n_missing := len;
        if len = 0 then eof := true
      );

      let b1, i1, len1 = In_buffered.fill_and_get ic in
      let len = min (Bytes.length self.buf.bytes) (min len1 !n_missing) in
      Bytes.blit b1 i1 self.buf.bytes 0 len;
      In_buffered.consume ic len;
      n_missing := !n_missing - len;
      self.buf.len <- len
    );
    0
  in
  let close () = In_buffered.close ic in

  In_buffered.create ~close ~fill_buffer ()
