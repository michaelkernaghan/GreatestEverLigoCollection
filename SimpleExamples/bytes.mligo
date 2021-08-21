let concat_op (s : bytes) : bytes =
   Bytes.concat s 0x7070

let slice_op (s : bytes) : bytes = Bytes.sub 1n 2n s

let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)

  let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)

  val length : bytes -> nat