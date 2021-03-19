open Monolith

module type Testable = sig
  type commit_metadata
  type commit
  type entry
  type node_tree
  type inode_value
  type inode_pointer
  type inode_tree

  val fixed_int : int -> string
  val leb128 : int -> string
  val contents : bytes -> string
  val commit_metadata : commit_metadata -> string
  val commit : commit -> string
  val entry : entry -> string
  val node_tree : node_tree -> string
  val inode_value : inode_value -> string
  val inode_pointer : inode_pointer -> string
  val inode_tree : inode_tree -> string
end

module C = struct
  type commit_metadata = Spec.commit_metadata
  type commit = string -> string list -> int64 -> string -> string -> string
  type entry = Spec.entry
  type node_tree = Spec.node_tree
  type inode_value = Spec.inode_value
  type inode_pointer = int -> string -> string
  type inode_tree = Spec.inode_tree

  let fixed_int = Spec.fixed_int
  let leb128_int = Spec.leb128_int
  let contents = Spec.contents
  let commit_metadata = Spec.commit_metadata

  let commit t p d a m =
    Spec.commit
      {
        tree = t;
        parents = p;
        metadata = { date = d; author = a; message = m };
      }

  (* let entry : entry -> string
      let node_tree : node_tree -> string
     let inode_value : inode_value -> string *)
  let inode_pointer i h = Spec.inode_pointer { index = i; hash = h }
  (*
     let inode_pointer : inode_pointer -> string
     let inode_tree : inode_tree -> string *)
end

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

  let commit_metadata _ = "todo"
  let commit _ _ _ _ _ = "todo"
  let inode_pointer _ _ = "todo"
end

module G = struct
  (* Custom generators absent from Monolith or specific to Spec *)
  let bytes () = Bytes.of_string (Gen.string (Gen.int (1 lsl 20)) Gen.char ())
  let int64 () = Random.int64 (Int64.of_int 42)

  let commit_metadata () =
    {
      Spec.date = int64 ();
      Spec.author = Gen.string (Gen.int (1 lsl 20)) Gen.char ();
      Spec.message = Gen.string (Gen.int (1 lsl 20)) Gen.char ();
    }
end

module P = struct
  let commit_metadata { Spec.date = d; Spec.author = a; Spec.message = m } =
    PPrint.string
      (Printf.sprintf "{ date = %s; author = %s; message = %s }"
         (Int64.to_string d) a m)
end

let bytes = easily_constructible G.bytes (fun _ -> PPrint.empty)
let string = deconstructible PPrint.string
let check_size s = String.length s > 10

let int64 =
  easily_constructible G.int64 (fun i -> PPrint.string (Int64.to_string i))

let commit_metadata = easily_constructible G.commit_metadata P.commit_metadata

let () =
  let spec = bytes ^> string in
  declare "contents" spec R.contents C.contents;

  let spec = int_within (Gen.int (1 lsl 20)) ^> string in
  declare "fixed int" spec R.fixed_int C.fixed_int;

  let spec = int_within (Gen.int (1 lsl 20)) ^> string in
  declare "LEB128 int" spec R.leb_int C.leb128_int;

  let spec = commit_metadata ^> string in
  declare "commit metadata" spec R.commit_metadata C.commit_metadata

(*
  let spec = string ^> list string ^> int64 ^> string ^> string ^> string in
  declare "commit" spec R.commit C.commit;

  let spec = int ^> string ^> string in
  declare "inode pointer" spec R.inode_pointer C.inode_pointer
 *)
let () =
  let fuel = 10 in
  main fuel
