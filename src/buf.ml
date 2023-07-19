(** tiny internal buffer implementation *)

type t = {
  mutable bytes: bytes;
  mutable len: int;  (** slice is [bytes[0..len]] *)
}

let create ?(size = 4_096) () : t = { bytes = Bytes.create size; len = 0 }
let[@inline] size self = self.len
let[@inline] bytes_slice self = self.bytes
let[@inline] clear self : unit = self.len <- 0

let resize self new_size : unit =
  let new_buf = Bytes.create new_size in
  Bytes.blit self.bytes 0 new_buf 0 self.len;
  self.bytes <- new_buf

let ensure self new_size : unit =
  (* need to resize? *)
  if new_size >= Bytes.length self.bytes then (
    (* amortized *)
    let new_cap = min Sys.max_string_length (new_size + (self.len / 2) + 10) in
    if new_cap < new_size then failwith "maximum bytes size exceeded";
    resize self new_cap
  )

let add_char (self : t) c : unit =
  ensure self (self.len + 1);
  Bytes.set self.bytes self.len c;
  self.len <- self.len + 1

let add_bytes (self : t) b i len : unit =
  ensure self (self.len + len);
  (* resize if needed? *)
  Bytes.blit b i self.bytes self.len len;
  self.len <- self.len + len

let contents (self : t) : string = Bytes.sub_string self.bytes 0 self.len
