open IO

val encode_out : ?chunk_size:int -> Out.t -> Out.t
(** Encode the written data as chunks.
    Each chunk is written as [<n>\r\n<n bytes>\r\n] where [<n>] is
    a hexadecimal representation of the number of bytes that follows.
    see https://tools.ietf.org/html/rfc7230#section-4.1 *)

val encode_in : ?chunk_size:int -> In.t -> In_buffered.t
(** Reader version of !{encode_out}, to transform an input stream into a chunked
    one *)

val decode_in : In_buffered.t -> In_buffered.t
(** Decode a chunk-encoded stream *)
