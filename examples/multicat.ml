
type op =
  | Rot13
  | Zip
  | Unzip
  | Chunk
  | Unchunk

let () =
  let ops = ref [] in
  let add_op o () = ops := o :: !ops in
  let stdin = ref false in
  let out = ref [] in
  let opts = [
    "-rot13", Arg.Unit (add_op Rot13), " rot13 translation";
    "-zip", Arg.Unit (add_op Zip), " zip compression";
    "-unzip", Arg.Unit (add_op Unzip), " zip decompression";
    "-chunk", Arg.Unit (add_op Chunk), " perform chunk encoding";
    "-unchunk", Arg.Unit (add_op Unchunk), " perform chunk decoding";
    "-stdin", Arg.Set stdin, " read from stdin";
    "-o", Arg.String (fun f -> out := f :: !out), " output to this file";
  ] |> Arg.align in
  let files = ref [] in
  Arg.parse opts (fun x -> files := x :: !files) "multicat [option*] [file]+\n\norder of options matters";
  IO_helpers.with_in_l (List.rev !files)
    (fun ic_l ->
       let ic_l = if !files=[] || !stdin then IO.In.of_unix_fd Unix.stdin :: ic_l else ic_l in
       let ic = IO_helpers.concat ic_l in
       let oc =
         match !out with
         | [] -> IO.Out.of_unix_fd ~bufsize:64 Unix.stdout
         | l ->
           List.map
             (fun f -> IO.Out.of_unix_fd @@
               Unix.openfile f [Unix.O_CREAT; Unix.O_WRONLY] 0o644)
             l
           |> IO_helpers.tee
       in
       let ic, oc =
         List.fold_left
           (fun (ic,oc) op ->
              match op with
              | Rot13 -> IO_helpers.rot13 ic, oc
              | Chunk -> ic, IO_helpers.Chunked_encoding.encode ~chunk_size:32 oc
              | Unchunk -> IO_helpers.Chunked_encoding.decode ic, oc
              | Zip -> ic, IO_helpers.Gzip.encode ~buf_size:1024 oc
              | Unzip -> IO_helpers.Gzip.decode ~buf_size:1024 ic, oc
           )
           (ic,oc) (List.rev !ops)
       in
       IO_helpers.copy ic oc;
       IO.Out.flush oc;
       IO.Out.close oc;
    );
  ()
