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
  | CODE
  | DLPT
  | DLLS
  | PRIM
  | DATA
  | SYMB
  | CRCS
  | DBUG
  | Unknown of string

let of_string = function
  | "CODE" -> CODE
  | "DLPT" -> DLPT
  | "DLLS" -> DLLS
  | "PRIM" -> PRIM
  | "DATA" -> DATA
  | "SYMB" -> SYMB
  | "CRCS" -> CRCS
  | "DBUG" -> DBUG
  | s -> Unknown s

let to_string = function
  | CODE -> "CODE"
  | DLPT -> "DLPT"
  | DLLS -> "DLLS"
  | PRIM -> "PRIM"
  | DATA -> "DATA"
  | SYMB -> "SYMB"
  | CRCS -> "CRCS"
  | DBUG -> "DBUG"
  | Unknown s ->
    if String.length s <> 4 then
      Fmt.failwith
        "OByteLib.Section.to_string: Sections should be only 4 characters: %s" s
    else s
