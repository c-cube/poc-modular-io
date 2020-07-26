
let () =
  let rot13 = ref false in
  let zip = ref false in
  let unzip = ref false in
  let stdin = ref false in
  let chunk = ref false in
  let unchunk = ref false in
  let out = ref [] in
  let opts = [
    "-rot13", Arg.Set rot13, " rot13 \"encoding\"";
    "-zip", Arg.Set zip, " zip compression";
    "-unzip", Arg.Set unzip, " zip decompression";
    "-chunk", Arg.Set chunk, " perform chunk encoding";
    "-unchunk", Arg.Set unchunk, " perform chunk decoding";
    "-stdin", Arg.Set stdin, " read from stdin";
    "-o", Arg.String (fun f -> out := f :: !out), " output to this file";
  ] |> Arg.align in
  let files = ref [] in
  Arg.parse opts (fun x -> files := x :: !files) "multicat [option*] [file]+";
  IO_helpers.with_in_l (List.rev !files)
    (fun ic_l ->
       let ic_l = if !files=[] || !stdin then IO.In.of_unix_fd Unix.stdin :: ic_l else ic_l in
       let ic = IO_helpers.concat ic_l in
       let ic = if !rot13 then IO_helpers.rot13 ic else ic in
       let ic = if !chunk then IO_helpers.Chunked_encoding.encode ~chunk_size:32 ic else ic in
       let ic = if !unchunk then IO_helpers.Chunked_encoding.decode ic else ic in
       let ic = if !zip then IO_helpers.Gzip.encode ic else ic in
       let ic = if !unzip then IO_helpers.Gzip.decode ic else ic in
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
       IO_helpers.copy ic oc;
       IO.Out.flush oc;
    );
  ()
