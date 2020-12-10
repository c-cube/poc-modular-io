open IO

let encode ?(chunk_size=4_096) oc : Out.t =
  let buf = Bytes.create chunk_size in
  let len = ref 0 in
  let write_header n =
    let header = Printf.sprintf "%x\r\n" n in
    Out.write_string oc header;
  in
  (* write the content of [buf] as a chunk *)
  let flush () =
    if !len > 0 then (
      write_header !len;
      Out.write oc buf 0 !len;
      len := 0;
      Out.write_string oc "\r\n";
    );
    Out.flush oc
  in
  let rec write buf2 i2 len2 =
    let n2 = min (chunk_size - !len) len2 in (* how much of [buf2] we can actually consume *)
    let size_chunk = !len + n2 in
    if size_chunk > 0 then (
      write_header size_chunk;
      if !len > 0 then (
        Out.write oc buf 0 !len;
        len := 0;
      );
      Out.write oc buf2 i2 n2;
      Out.write_string oc "\r\n";
      (write[@tailcall]) buf2 (i2 + n2) (len2 - n2);
    )
  in
  let close () =
    flush ();
    Out.write_string oc "0\r\n"; (* empty chunk *)
    Out.flush oc;
    Out.close oc
  and write_char c =
    if !len = chunk_size then flush();
    assert (!len < chunk_size);
    Bytes.set buf !len c;
    incr len
  in
  Out.of_funs ~write_char ~write ~flush ~close ()

let encode_in ?(chunk_size=4_096) ic : In.t =
  let buf = Bytes.create (chunk_size+2) in (* have space for chunk+\r\n *)
  (* state machine: writing header, or writing chunk *)
  let writing_header = ref true in
  let i = ref 0 in
  let len = ref 0 in
  let consume n =
    i := !i + n;
    len := !len - n;
    if n > 0 && !len = 0 then (
      writing_header := not !writing_header
    );
  and close () = In.close ic
  and fill_buf () =
    if !len = 0 then (
      i := 0;
      let b1, i1, len1 = In.fill_buf ic in
      let len1 = min len1 chunk_size in
      if !writing_header then (
        let ch = Printf.sprintf "%x\r\n" len1 in
        Bytes.blit_string ch 0 buf 0 (String.length ch);
        len := String.length ch;
      ) else if len1 > 0 then (
        (* only write a chunk if it's non empty *)
        Bytes.blit b1 i1 buf 0 len1;
        In.consume ic len1; (* consume chunk from input *)
        Bytes.set buf len1 '\r';
        Bytes.set buf (len1+1) '\n';
        len := len1 + 2;
      );
    );
    buf, !i, !len
  in
  In.of_funs ~consume ~close ~fill_buf ()

let decode ic : In.t =
  let first = ref true in
  let eof = ref false in
  let read_next_chunk_len () : int =
    if !first then (
      first := false
    ) else (
      match IO_helpers.read_line ic with
      | None -> failwith "expected crlf between chunks"
      | Some "\r" -> ()
      | Some s ->
        failwith @@ Printf.sprintf "expected crlf between chunks, not %S" s
    );
    (* NOTE: we could re-use some buffer for reading the (short) lines *)
    match IO_helpers.read_line ic with
    | None -> failwith "expected <length crlf> at start of chunk"
    | Some line ->
      (* parse length, drop optional extension *)
      try Scanf.sscanf line "%x %s@\r" (fun n _ext -> n)
      with _ -> failwith @@ "cannot read chunk size from line " ^ line
  in
  (* how many bytes remain to be read from current chunk *)
  let n_missing = ref 0 in
  let fill_buf () =
    if !n_missing = 0 && not !eof then (
      (* start of chunk *)
      let len = read_next_chunk_len () in
      n_missing := len;
      if len = 0 then eof := true;
    );

    let b1, i1, len1 = In.fill_buf ic in
    let len1 = min len1 !n_missing in
    b1, i1, len1
  and consume n =
    assert (n <= !n_missing);
    n_missing := !n_missing - n;
    In.consume ic n
  and close () = In.close ic in
  In.of_funs ~consume ~close ~fill_buf ()
