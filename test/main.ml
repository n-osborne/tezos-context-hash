open Monolith

module type Testable = sig
  val fixed_int : int -> string
  val contents : bytes -> string
end

module C = Spec

module R = struct
  open Irmin.Type

  let fixed_int i =
    let buf = Bytes.create 8 in
    unstage (encode_bin int64) (Int64.of_int i) (fun s ->
        Bytes.blit_string s 0 buf 0 8);
    Bytes.unsafe_to_string buf

  let leb_int i =
    let buf = Buffer.create 8 in
    unstage (encode_bin int) i (Buffer.add_string buf);
    Buffer.contents buf

  let contents b =
    let buf = Buffer.create (8 + Bytes.length b) in
    (unstage (pre_hash Irmin_tezos.Encoding.Contents.t))
      b (Buffer.add_string buf);
    Buffer.contents buf
end

let bytes =
  easily_constructible
    (fun () -> Bytes.of_string (Gen.string (Gen.int (1 lsl 20)) Gen.char ()))
    (fun _ -> PPrint.empty)

let string = deconstructible PPrint.string
let check_size s = String.length s > 10

let () =
  let spec = bytes ^> string in
  declare "contents" spec R.contents C.contents;

  let spec = int_within (Gen.int (1 lsl 20)) ^> string in
  declare "fixed int" spec R.fixed_int C.fixed_int;

  let spec = int_within (Gen.int (1 lsl 20)) ^> string in
  declare "LEB128 int" spec R.leb_int C.leb_int

let () =
  let fuel = 10 in
  main fuel
