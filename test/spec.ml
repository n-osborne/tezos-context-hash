(** {1 Specification implementation}

    This module implements the specification present in SPEC.md, independently
    of the Irmin implementaiton. This code is not intended to be efficient, but
    rather tries to make the correspondance with the specification as clear as
    possible. For this reason, this code should not be used in production. *)

(** {2 Integers}

    See section `Integers` of SPEC.md *)

let fixed_int64 i =
  let buf = Buffer.create 8 in
  Buffer.add_int64_be buf i;
  Buffer.contents buf

let fixed_int i = Int64.of_int i |> fixed_int64

let leb128_int i =
  (* The final size of the result may be smaller (but not greater) than 8. *)
  let buf = Buffer.create 8 in
  let rec loop i =
    if i = 0 then Buffer.contents buf
    else
      let b = i land 127 in
      let i = i lsr 7 in
      (if i <> 0 then b lor 128 else b) |> Buffer.add_uint8 buf;
      loop i
  in
  loop i

(** {2 Contents}

    See section `Contents` of SPEC.md *)

let contents c =
  let buf = Buffer.create (8 + Bytes.length c) in
  Buffer.add_string buf (fixed_int (Bytes.length c));
  Buffer.add_bytes buf c;
  Buffer.contents buf

(** {2 Commits}

    See the section `Commits` of SPEC.md *)

type commit_metadata = { date : int64; author : string; message : string }

let commit_metadata { date; author; message } =
  let buf =
    Buffer.create (8 + (8 + String.length author) + (8 + String.length message))
  in
  Buffer.add_string buf (fixed_int64 date);
  Buffer.add_string buf (fixed_int (String.length author));
  Buffer.add_string buf author;
  Buffer.add_string buf (fixed_int (String.length message));
  Buffer.add_string buf message;
  Buffer.contents buf

type hash = string
type commit = { tree : hash; parents : hash list; metadata : commit_metadata }

let commit { tree; parents; metadata } =
  let encoded_metadata = commit_metadata metadata in
  let buf =
    Buffer.create
      (8
      + 32
      + 8
      + (List.length parents * (32 + 8))
      + String.length encoded_metadata)
  in
  Buffer.add_string buf (fixed_int 32);
  Buffer.add_string buf tree;
  Buffer.add_string buf (fixed_int (List.length parents));
  List.iter
    (fun h ->
      Buffer.add_string buf (fixed_int 32);
      Buffer.add_string buf h)
    parents;
  Buffer.add_string buf encoded_metadata;
  Buffer.contents buf

(** {2 Trees}

    See section `Trees` of SPEC.md *)

type entry_kind = Content | Node
type entry = { name : string; kind : entry_kind; hash : hash }

(** {3 Nodes}

    When a tree contains less than 256 entries, it is encoded as a flat list of
    entries

    See section `Nodes` of SPEC.md *)

type node_tree = entry list

let entry { name; kind; hash } =
  let leb128_name_len = leb128_int (String.length name) in
  let buf =
    Buffer.create
      (8 + String.length leb128_name_len + String.length name + 8 + 32)
  in
  Buffer.add_string buf
    (match kind with
    | Content -> "\255\000\000\000\000\000\000\000"
    | Node -> "\000\000\000\000\000\000\000\000");
  Buffer.add_string buf leb128_name_len;
  Buffer.add_string buf name;
  Buffer.add_string buf (fixed_int 32);
  Buffer.add_string buf hash;
  Buffer.contents buf

let node_tree t =
  assert (List.length t < 256);
  let buf = Buffer.create 8 (* + lengths of the encoded entries *) in
  Buffer.add_string buf (fixed_int (List.length t));
  List.sort (fun e e' -> String.compare e.name e'.name) t
  |> List.iter (fun e -> Buffer.add_string buf (entry e));
  Buffer.contents buf

(** {3 Inodes}

    Trees that contain 256 entries or more are first transforned into inodes,
    before their final encoding.

    See section `Inodes values` of SPEC.md *)

let entry { name; kind; hash } =
  let leb128_name_len = leb128_int (String.length name) in
  let buf =
    Buffer.create (String.length leb128_name_len + String.length name + 1 + 32)
  in
  Buffer.add_string buf leb128_name_len;
  Buffer.add_string buf name;
  Buffer.add_char buf (match kind with Content -> '\001' | Node -> '\000');
  Buffer.add_string buf hash;
  Buffer.contents buf

type inode_value = entry list

let inode_value v =
  assert (List.length v < 32);
  let buf = Buffer.create (1 + 1 (* + lengths of the encoded entries *)) in
  Buffer.add_char buf '\000';
  Buffer.add_char buf (Char.chr (List.length v));
  List.sort (fun e e' -> String.compare e.name e'.name) v
  |> List.iter (fun e -> Buffer.add_string buf (entry e));
  Buffer.contents buf

type inode_pointer = { index : int; hash : hash }

let inode_pointer { index; hash } =
  assert (index < 32);
  let buf = Buffer.create (1 + 32) in
  Buffer.add_char buf (Char.chr index);
  Buffer.add_string buf hash;
  Buffer.contents buf

type inode_tree = {
  depth : int;
  entries_length : int;
  pointers : inode_pointer list;
}

let inode_tree { depth; entries_length; pointers } =
  assert (List.length pointers < 32);
  let leb128_depth = leb128_int depth in
  let leb128_entries_length = leb128_int entries_length in
  let buf =
    Buffer.create
      (1
      + String.length leb128_depth
      + String.length leb128_entries_length
      + 1
      + (33 * List.length pointers))
  in
  Buffer.add_char buf '\001';
  Buffer.add_string buf leb128_depth;
  Buffer.add_string buf leb128_entries_length;
  Buffer.add_char buf (Char.chr (List.length pointers));
  List.sort (fun p p' -> Int.compare p.index p'.index) pointers
  |> List.iter (fun p -> Buffer.add_string buf (inode_pointer p));
  Buffer.contents buf

(** {4 Internal tree construction} *)

let ocaml_hash seed s =
  (* TODO *)
  Hashtbl.seeded_hash seed s

let index d n = ocaml_hash d n mod 32
