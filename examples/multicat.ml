open Io

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
  let opts =
    [
      "-rot13", Arg.Unit (add_op Rot13), " rot13 translation";
      "-zip", Arg.Unit (add_op Zip), " zip compression";
      "-unzip", Arg.Unit (add_op Unzip), " zip decompression";
      "-chunk", Arg.Unit (add_op Chunk), " perform chunk encoding";
      "-unchunk", Arg.Unit (add_op Unchunk), " perform chunk decoding";
      "-stdin", Arg.Set stdin, " read from stdin";
      "-o", Arg.String (fun f -> out := f :: !out), " output to this file";
    ]
    |> Arg.align
  in
  let files = ref [] in
  Arg.parse opts
    (fun x -> files := x :: !files)
    "multicat [option*] [file]+\n\norder of options matters";
  IO_helpers.with_in_l (List.rev !files) (fun ic_l ->
      let ic_l =
        if !files = [] || !stdin then
          IO.In.of_unix_fd Unix.stdin :: ic_l
        else
          ic_l
      in
      let ic = IO_helpers.concat ic_l in
      let oc =
        match !out with
        | [] -> IO.Out.of_out_channel stdout
        | l ->
          List.map
            (fun f ->
              IO.Out.of_out_channel
              @@ open_out_gen [ Open_creat; Open_wronly ] 0o644 f)
            l
          |> IO_helpers.tee
      in
      let ic, oc =
        List.fold_left
          (fun (ic, oc) op ->
            match op with
            | Rot13 -> Rot13.map_in ic, oc
            | Chunk -> ic, Chunked_encoding.encode_out oc
            | Unchunk ->
              let ic = IO.In_buffered.of_in ic in
              Chunked_encoding.decode_in ic |> IO.In_buffered.into_in, oc
            | Zip -> ic, IO_gzip.encode ~buf_size:1024 oc
            | Unzip -> IO_gzip.decode ~buf_size:1024 ic, oc)
          (ic, oc) (List.rev !ops)
      in
      IO_helpers.copy ic oc;
      IO.Out.flush oc;
      IO.Out.close oc);
  ()
