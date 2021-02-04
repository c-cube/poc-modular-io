
open IO

let decode ?(buf_size=4096 * 32) (ic:In.t) : In.t =
  let buf = Bytes.create buf_size in
  let buf_len = ref 0 in
  let write_offset = ref 0 in
  let zlib_str = Zlib.inflate_init false in
  let is_done = ref false in
  let close () =
    Zlib.inflate_end zlib_str;
    In.close ic
  in
  let consume len : unit =
    if len > !buf_len then (
      failwith @@ Printf.sprintf
        "inflate: error during decompression: invalid consume len %d (max %d)"
        len !buf_len
    );
    write_offset := !write_offset + len;
  in
  let fill_buf () : _*_*_ =
    (* refill [buf] if needed *)
    if !write_offset >= !buf_len && not !is_done then (
      let ib, ioff, ilen = In.fill_buf ic in
      begin
        try
          let finished, used_in, used_out =
            Zlib.inflate zlib_str
              ib ioff ilen
              buf 0 (Bytes.length buf)
              Zlib.Z_SYNC_FLUSH
          in
          In.consume ic used_in;
          write_offset := 0;
          buf_len := used_out;
          if finished then is_done := true;
        with Zlib.Error (e1,e2) ->
          failwith @@ Printf.sprintf
            "inflate: error during decompression:\n%s %s" e1 e2
      end;
    );
    buf, !write_offset, !buf_len - !write_offset
  in
  In.of_funs ~fill_buf ~consume ~close ()

let encode ?(buf_size=4096 * 32) (oc:Out.t) : Out.t =
  let w_buf = Bytes.create buf_size in
  let w_len = ref 0 in
  let buf0 = Bytes.create 1 in
  let zlib_str = Zlib.deflate_init 4 false in

  let rec write buf i len =
    if len > 0 then (
      assert (!w_len <= buf_size);
      if !w_len = buf_size then (
        (* need to make room *)
        Out.write oc w_buf 0 !w_len;
        w_len := 0;
      );
      let _end, used_in, written =
        Zlib.deflate zlib_str
          buf i len
          w_buf !w_len (buf_size - !w_len)
          Zlib.Z_NO_FLUSH
      in
      w_len := !w_len + written;
      write buf (i + used_in) (len - used_in)
    )
  in
  let flush () =
    if !w_len > 0 then (
      Out.write oc w_buf 0 !w_len;
      w_len := 0;
    );
    let rec flush_loop () =
      let _finished, _used_in, used_out =
        Zlib.deflate zlib_str
          buf0 0 0
          w_buf 0 buf_size
          Zlib.Z_FULL_FLUSH
      in
      if used_out > 0 then (
        Out.write oc w_buf 0 used_out;
        flush_loop()
      )
    in
    flush_loop();
    Out.flush oc;
  and write_char c =
    Bytes.set buf0 0 c;
    write buf0 0 1
  and close() =
    ignore @@ Zlib.deflate zlib_str
      buf0 0 0
      w_buf 0 buf_size
      Zlib.Z_FINISH
    ;
    Zlib.deflate_end zlib_str;
    Out.close oc
  in
  Out.of_funs ~write_char ~write ~flush ~close ()

let encode_in ?(buf_size=4096 * 32) (ic:In.t) : In.t =
  let refill = ref true in
  let buf = Bytes.create buf_size in
  let buf_len = ref 0 in
  let write_offset = ref 0 in
  let zlib_str = Zlib.deflate_init 4 false in
  let close () =
    Zlib.deflate_end zlib_str;
    In.close ic
  and consume n =
    write_offset := n + !write_offset
  and fill_buf () =
    let rec loop() =
      if !write_offset < !buf_len then (
        (* still the same slice, not consumed entirely by output *)
        buf, !write_offset, !buf_len - !write_offset
      ) else if not !refill then (
        (* empty slice, no refill *)
        buf, !write_offset, !buf_len - !write_offset
      ) else (
        (* the output was entirely consumed, we need to do more work *)
        write_offset := 0;
        buf_len := 0;
        let in_s, in_i, in_len = In.fill_buf ic in
        if in_len>0 then (
          (* try to decompress from input buffer *)
          let _finished, used_in, used_out =
            Zlib.deflate zlib_str
              in_s in_i in_len
              buf 0 (Bytes.length buf)
              Zlib.Z_NO_FLUSH
          in
          buf_len := used_out;
          In.consume ic used_in;
          if _finished then (
            refill := false;
          );
          loop()
        ) else (
          (* finish sending the internal state *)
          let _finished, used_in, used_out =
            Zlib.deflate zlib_str
              in_s in_i in_len
              buf 0 (Bytes.length buf)
              Zlib.Z_FULL_FLUSH
          in
          assert (used_in = 0);
          buf_len := used_out;
          if used_out = 0 then (
            refill := false;
          );
          loop()
        )
      )
    in
    try loop()
    with Zlib.Error (e1,e2) ->
      failwith @@ Printf.sprintf
        "deflate: error during compression:\n%s %s" e1 e2
  in
  In.of_funs ~fill_buf ~consume ~close ()
