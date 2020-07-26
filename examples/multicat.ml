
let () =
  let rot13 = ref false in
  let zip = ref false in
  let unzip = ref false in
  let stdin = ref false in
  let out = ref "" in
  let opts = [
    "-rot13", Arg.Set rot13, " rot13 \"encoding\"";
    "-zip", Arg.Set zip, " zip compression";
    "-unzip", Arg.Set unzip, " zip decompression";
    "-stdin", Arg.Set stdin, " read from stdin";
    "-o", Arg.Set_string out, " output file";
  ] |> Arg.align in
  let files = ref [] in
  Arg.parse opts (fun x -> files := x :: !files) "multicat [option*] [file]+";
  IO_helpers.with_in_l (List.rev !files)
    (fun ic_l ->
       let ic_l = if !stdin then IO.In.of_unix_fd Unix.stdin :: ic_l else ic_l in
       let ic = IO_helpers.concat ic_l in
       let ic = if !rot13 then IO_helpers.rot13 ic else ic in
       let ic = if !zip then IO_helpers.Gzip.encode ic else ic in
       let ic = if !unzip then IO_helpers.Gzip.decode ic else ic in
       match !out with
       | "" ->
         let oc = IO.Out.of_unix_fd Unix.stdout in
         IO_helpers.copy ic oc;
         IO.Out.flush oc
       | f_out ->
         IO_helpers.with_out f_out
           (fun oc ->
             IO_helpers.copy ic oc;
             IO.Out.flush oc));
  ()
