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

let pp_ml_array pp_v fmt array =
  Fmt.pf fmt "[%a]" (Fmt.array ~sep:(fun fmt () -> Fmt.char fmt ';') pp_v) array

let input_binary_int_rev =
  let buf4 = Bytes.create 4 in
  fun ic ->
    match In_channel.really_input ic buf4 0 4 with
    | None -> Fmt.failwith "Tools.input_binary_int_rev"
    | Some () -> (
      let n =
        int_of_char (Bytes.get buf4 0)
        lor (int_of_char (Bytes.get buf4 1) lsl 8)
        lor (int_of_char (Bytes.get buf4 2) lsl 16)
        lor (int_of_char (Bytes.get buf4 3) lsl 24)
      in
      match Sys.word_size with
      | 32 -> n
      | 64 -> (n lsl 32) asr 32
      | ws -> Fmt.failwith "unsupported architecture: sizeof(word) = %d" ws )

let find_object_method o tag =
  let rec bin_search tbl tag li hi =
    if li >= hi then Obj.field tbl (li - 1)
    else
      let mi = ((li + hi) lsr 1) lor 1 in
      let tag' = Obj.raw_field tbl mi in
      if Nativeint.compare tag tag' < 0 then bin_search tbl tag li (mi - 2)
      else bin_search tbl tag mi hi
  in
  let tbl = Obj.field o 0 in
  let hi = ((Obj.obj (Obj.field tbl 0) : int) lsl 1) lor 1 in
  bin_search tbl tag 3 hi
