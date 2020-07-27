
open IO

let in_of_bytes ?(off=0) ?len s : In.t =
  (* invariant: !i+!len is constant *)
  let i = ref off in
  let len =
    ref (
      match len with
      | Some n ->
        if n > Bytes.length s - off then (
          invalid_arg "IO.in_of_bytes";
        );
        n
      | None -> Bytes.length s - off
    )
  in

  let consume n = assert (n>=0 && n<= !len); i := !i + n; len := !len - n in
  let fill_buf () =s, !i, !len in
  let close () = len := 0 in
  In.of_funs ~consume ~close ~fill_buf ()

let in_of_string ?off ?len s : In.t =
  in_of_bytes ?off ?len (Bytes.unsafe_of_string s)

let out_of_buffer buf : Out.t =
  let write_char = Buffer.add_char buf in
  let write = Buffer.add_subbytes buf in
  let flush() = () in
  let close() = () in
  Out.of_funs ~write ~write_char ~flush ~close ()

type filename = string

let with_in name f =
  let fd = Unix.openfile name [Unix.O_RDONLY] 0o644 in
  Fun.protect
    ~finally:(fun() -> Unix.close fd)
    (fun () -> f @@ In.of_unix_fd fd)

let with_in_l names f =
  let opened = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close !opened)
    (fun () ->
       let l =
         List.map
         (fun name ->
              let fd = Unix.openfile name [Unix.O_RDONLY] 0o644 in
              opened := fd :: !opened;
              In.of_unix_fd fd)
           names
       in
       f l)

let with_out name f =
  let fd = Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
  Fun.protect
    ~finally:(fun() -> Unix.close fd)
    (fun () ->
       let oc = Out.of_unix_fd fd in
       let x = f oc in
       Out.flush oc;
       x)

let concat l0 =
  let l = ref l0 in
  let rec fill_buf () =
    match !l with
    | [] -> Bytes.empty, 0, 0
    | ic :: tl ->
      let bs, i, len = In.fill_buf ic in
      if len = 0 then (
        l := tl;
        fill_buf ()
      ) else (
        bs, i, len
      )
  in
  let close () = List.iter In.close l0
  and consume n =
    match !l with
    | _ when n=0 -> ()
    | ic :: _ -> In.consume ic n
    | [] -> failwith "cannot consume empty `concat` stream"
  in
  In.of_funs ~consume ~fill_buf ~close ()

let tee l =
  let write bs i len =
    List.iter (fun oc -> IO.Out.write oc bs i len) l
  and write_char c =
    List.iter (fun oc -> IO.Out.write_char oc c) l
  and close () =
    List.iter IO.Out.close l
  and flush () =
    List.iter IO.Out.flush l
  in
  IO.Out.of_funs ~write_char ~write ~close ~flush ()

let copy ic oc : unit =
  let continue = ref true in
  while !continue do
    let bs, i, len = In.fill_buf ic in
    if len = 0 then (
      continue := false;
    ) else (
      Out.write oc bs i len;
      In.consume ic len;
    )
  done

let read_line (ic:In.t) : string option =
  (* find index of [c] in slice, or raise [Not_found] *)
  let index_in_slice_ buf i len c : int =
    let j = Bytes.index_from buf i c in
    if j < i+len then j else raise Not_found
  in

  (* see if we can directly extract a line from current buffer *)
  let bs, i, len = In.fill_buf ic in
  if len = 0 then None else (
    match index_in_slice_ bs i len '\n' with
    | j ->
      (* easy case: buffer already contains a full line *)
      let line = Bytes.sub_string bs i (j-i) in
      In.consume ic (j-i+1);
      Some line
    | exception Not_found ->
    (* create new buffer, already filled with beginning of line *)
      let buf = Buffer.create 256 in
      Buffer.add_subbytes buf bs i len;
      In.consume ic len;
      let continue = ref true in
      while !continue do
        let bs, i, len = In.fill_buf ic in
        if len=0 then (
          continue := false; (* EOF *)
        );
        match index_in_slice_ bs i len '\n' with
        | j ->
          Buffer.add_subbytes buf bs i (j-i); (* without '\n' *)
          In.consume ic (j-i+1); (* consume, including '\n' *)
          continue := false
        | exception Not_found ->
          Buffer.add_subbytes buf bs i len;
          In.consume ic len;
      done;
      Some (Buffer.contents buf)
  )

let map_c ?(buf_size=256) f (ic:In.t) : In.t =
  (* cannot modify input buffer in place because we don't control
     consumer side, so the same slice might be used in several calls
     and [f] might not be idempotent (see {!rot13}) *)
  let buf = Bytes.create buf_size in
  let i = ref 0 in
  let len = ref 0 in
  let close () = In.close ic
  and consume n =
    i := !i + n;
    len := !len - n;
  and fill_buf () =
    assert (!len >= 0);
    if !len = 0 then (
      (* need to refill *)
      i := 0;
      let bs1, i1, len1 = In.fill_buf ic in
      let len' = min len1 buf_size in
      Bytes.blit bs1 i1 buf 0 len';
      In.consume ic len';
      for j=0 to len' - 1 do
        Bytes.set buf j (f (Bytes.get buf j));
      done;

      len := len';
    );
    buf, !i, !len
  in
  In.of_funs ~close ~consume ~fill_buf ()

let rot13 ?buf_size ic =
  let f c =
    match c with
    | 'a' .. 'z' ->
      Char.chr (Char.code 'a' + ((Char.code c - Char.code 'a') + 13) mod 26)
    | 'A' .. 'Z' ->
      Char.chr (Char.code 'A' + ((Char.code c - Char.code 'A') + 13) mod 26)
    | c -> c
  in
  map_c ?buf_size f ic

let read_lines ic =
  let rec loop l = match read_line ic with
    | None -> List.rev l
    | Some s -> loop (s :: l)
  in
  loop []

let read_at_most ~close_rec max_size (ic:In.t) : In.t =
  let size = ref 0 in
  let continue = ref true in
  let fill_buf() =
    if !continue then In.fill_buf ic else Bytes.empty, 0, 0
  and close () =
    if close_rec then In.close ic
  and consume n =
    size := !size + n;
    if !size > max_size then (
      continue := false;
    ) else (
      In.consume ic n
    )
  in
  In.of_funs ~close ~fill_buf ~consume ()

let read_all_into ic buf =
  let oc = out_of_buffer buf in
  copy ic oc

let read_all ic =
  let buf = Buffer.create 256 in
  read_all_into ic buf;
  Buffer.contents buf

module Chunked_encoding = struct
  let encode ?(chunk_size=4_096) ic : In.t =
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
        match read_line ic with
        | None -> failwith "expected crlf between chunks"
        | Some "\r" -> ()
        | Some s ->
          failwith @@ Printf.sprintf "expected crlf between chunks, not %S" s
      );
      (* NOTE: we could re-use some buffer for reading the (short) lines *)
      match read_line ic with
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
    and close () = In.close ic
    in
    In.of_funs ~consume ~close ~fill_buf ()
end

module Gzip = struct
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

  let encode ?(buf_size=4096 * 32) (ic:In.t) : In.t =
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
end
