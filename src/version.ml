(*************************************************************************)
(*                                                                       *)
(*                              OByteLib                                 *)
(*                                                                       *)
(*                            Benoit Vaugon                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

type t =
  | V008
  | V010
  | V011
  | V022
  | V023
  | V025
  | V026
  | V027
  | V028
  | V029
  | V030
  | V031

let versions =
  [ V008; V010; V011; V022; V023; V025; V026; V027; V028; V029; V030; V031 ]

let to_string v =
  match v with
  | V008 -> "008"
  | V010 -> "010"
  | V011 -> "011"
  | V022 -> "022"
  | V023 -> "023"
  | V025 -> "025"
  | V026 -> "026"
  | V027 -> "027"
  | V028 -> "028"
  | V029 -> "029"
  | V030 -> "030"
  | V031 -> "031"

let to_magic v = Fmt.str "Caml1999X%s" (to_string v)

let magic_size =
  let len = String.length (to_magic V008) in
  let check_len v = String.length (to_magic v) = len in
  assert (List.for_all check_len versions);
  len

let of_magic s = List.find_opt (fun v -> String.equal s (to_magic v)) versions

let read ic =
  let file_size = In_channel.length ic in
  if Int64.compare file_size (Int64.of_int magic_size) < 0 then
    Fmt.failwith "too short file";
  let () = In_channel.seek ic (Int64.sub file_size (Int64.of_int magic_size)) in
  let magic_string = Bytes.create magic_size in
  match In_channel.really_input ic magic_string 0 magic_size with
  | None -> Fmt.failwith "Version.read"
  | Some () -> (
    let magic_string = Bytes.to_string magic_string in
    match of_magic magic_string with
    | None -> Fmt.failwith "unknown magic string: %S" magic_string
    | Some v -> v )

let write oc v = Out_channel.output_string oc (to_magic v)
