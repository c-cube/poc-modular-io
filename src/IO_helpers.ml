
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

let read_line (self:In.t) : string option =
  let open In in

  (* find index of [c] in slice, or raise [Not_found] *)
  let index_in_slice_ buf i len c : int =
    let j = Bytes.index_from buf i c in
    if j < i+len then j else raise Not_found
  in

  (* see if we can directly extract a line from current buffer *)
  let bs, i, len = fill_buf self in
  if len = 0 then None else (
    match index_in_slice_ bs i len '\n' with
    | j ->
      let line = Bytes.sub_string bs i (j-i) in
      In.consume self (j-i+1);
      Some line
    | exception Not_found ->
      let buf = Buffer.create 256 in
      Buffer.add_subbytes buf bs i len;
      let continue = ref true in
      while !continue do
        let bs, i, len = fill_buf self in
        if len=0 then (
          continue := false; (* EOF *)
        );
        match index_in_slice_ bs i len '\n' with
        | j ->
          Buffer.add_subbytes buf bs i (j-i); (* without '\n' *)
          consume self (j-i+1); (* consume, including '\n' *)
          continue := false
        | exception Not_found ->
          Buffer.add_subbytes buf bs i len;
          consume self len;
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
      let bs1, i1, len1 = In.fill_buf ic in
      let len' = min len1 buf_size in
      Bytes.blit bs1 i1 buf 0 len';
      In.consume ic len';
      for j=i1 to len' - 1 do
        Bytes.set buf j (f (Bytes.get buf j));
      done;

      i := 0;
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
                buf 0 (Bytes.length buf)
                ib ioff ilen Zlib.Z_SYNC_FLUSH
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
