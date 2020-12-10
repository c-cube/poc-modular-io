
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
  done;
  Out.flush oc

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

let map_out_c f (oc:Out.t) : Out.t =
  let write_char c = Out.write_char oc (f c)
  and write buf i len =
    for j=i to i+len-1 do
      Bytes.set buf j (f (Bytes.get buf j))
    done;
    Out.write oc buf i len
  and flush () = Out.flush oc
  and close () = Out.close oc in
  Out.of_funs ~write_char ~write ~close ~flush ()

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

