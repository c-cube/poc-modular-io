open IO

let f c =
  match c with
  | 'a' .. 'z' ->
    Char.chr (Char.code 'a' + ((Char.code c - Char.code 'a') + 13) mod 26)
  | 'A' .. 'Z' ->
    Char.chr (Char.code 'A' + ((Char.code c - Char.code 'A') + 13) mod 26)
  | c -> c

let map_in ?buf_size ic =
  IO_helpers.map_c ?buf_size f ic

let map_out oc =
  IO_helpers.map_out_c f oc
