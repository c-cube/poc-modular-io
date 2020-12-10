
open IO

val encode : ?chunk_size:int -> Out.t -> Out.t
(** Encode the written data as chunks.
    Each chunk is written as [<n>\r\n<n bytes>\r\n] where [<n>] is
    a hexadecimal representation of the number of bytes that follows.
    see https://tools.ietf.org/html/rfc7230#section-4.1 *)

val decode : In.t -> In.t
(** Read a chunk-encoded stream *)

val encode_in : ?chunk_size:int -> In.t -> In.t
(** Reader version of !{decode}, to transform an input stream into a chunked
    one *)
