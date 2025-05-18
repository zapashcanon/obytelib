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

type unop =
  | NOT
  | NEG
  | OFFSET of int
  | VECTLENGTH
  | ISINT

type binop =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | XOR
  | LSL
  | LSR
  | ASR

type compop =
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | ULT
  | UGE

type t =
  | ACC of int
  | PUSH
  | POP of int
  | ASSIGN of int
  | ENVACC of int
  | PUSH_RETADDR of int
  | APPLY of int
  | APPTERM of int * int
  | RETURN of int
  | RESTART
  | GRAB of int
  | CLOSURE of int * int
  | CLOSUREREC of int * int array
  | OFFSETCLOSURE of int
  | GETGLOBAL of int
  | SETGLOBAL of int
  | ATOM of int
  | MAKEBLOCK of int * int
  | MAKEFLOATBLOCK of int
  | GETFIELD of int
  | GETFLOATFIELD of int
  | SETFIELD of int
  | SETFLOATFIELD of int
  | GETVECTITEM
  | SETVECTITEM
  | GETBYTESCHAR
  | SETBYTESCHAR
  | GETSTRINGCHAR
  | BRANCH of int
  | BRANCHIF of int
  | BRANCHIFNOT of int
  | SWITCH of int array * int array
  | PUSHTRAP of int
  | POPTRAP
  | RAISE
  | RERAISE
  | RAISE_NOTRACE
  | CHECK_SIGNALS
  | C_CALL of int * int
  | CONSTINT of int
  | UNAPP of unop
  | BINAPP of binop
  | COMPARE of compop
  | COMPBRANCH of compop * int * int
  | OFFSETREF of int
  | GETMETHOD
  | GETPUBMET of int
  | GETDYNMET
  | STOP

(***)

let pp_unop fmt unop =
  match unop with
  | NOT -> Fmt.pf fmt "NOT"
  | NEG -> Fmt.pf fmt "NEG"
  | OFFSET n -> Fmt.pf fmt "OFFSET %d" n
  | VECTLENGTH -> Fmt.pf fmt "VECTLENGTH"
  | ISINT -> Fmt.pf fmt "ISINT"

let pp_binop fmt binop =
  match binop with
  | ADD -> Fmt.pf fmt "ADD"
  | SUB -> Fmt.pf fmt "SUB"
  | MUL -> Fmt.pf fmt "MUL"
  | DIV -> Fmt.pf fmt "DIV"
  | MOD -> Fmt.pf fmt "MOD"
  | AND -> Fmt.pf fmt "AND"
  | OR -> Fmt.pf fmt "OR"
  | XOR -> Fmt.pf fmt "XOR"
  | LSL -> Fmt.pf fmt "LSL"
  | LSR -> Fmt.pf fmt "LSR"
  | ASR -> Fmt.pf fmt "ASR"

let pp_compop fmt compop =
  match compop with
  | EQ -> Fmt.pf fmt "EQ"
  | NEQ -> Fmt.pf fmt "NEQ"
  | LT -> Fmt.pf fmt "LT"
  | LE -> Fmt.pf fmt "LE"
  | GT -> Fmt.pf fmt "GT"
  | GE -> Fmt.pf fmt "GE"
  | ULT -> Fmt.pf fmt "ULT"
  | UGE -> Fmt.pf fmt "UGE"

let pp pp_ptr pp_cfun pp_data fmt instr =
  let open Tools in
  match instr with
  | ACC n -> Fmt.pf fmt "ACC %d" n
  | PUSH -> Fmt.pf fmt "PUSH"
  | POP n -> Fmt.pf fmt "POP %d" n
  | ASSIGN n -> Fmt.pf fmt "ASSIGN %d" n
  | ENVACC n -> Fmt.pf fmt "ENVACC %d" n
  | PUSH_RETADDR ptr -> Fmt.pf fmt "PUSH_RETADDR %a" pp_ptr ptr
  | APPLY n -> Fmt.pf fmt "APPLY %d" n
  | APPTERM (n, s) -> Fmt.pf fmt "APPTERM %d %d" n s
  | RETURN n -> Fmt.pf fmt "RETURN %d" n
  | RESTART -> Fmt.pf fmt "RESTART"
  | GRAB n -> Fmt.pf fmt "GRAB %d" n
  | CLOSURE (n, ptr) -> Fmt.pf fmt "CLOSURE %d %a" n pp_ptr ptr
  | CLOSUREREC (n, ptrs) ->
    Fmt.pf fmt "CLOSUREREC %d %a" n (pp_ml_array pp_ptr) ptrs
  | OFFSETCLOSURE n -> Fmt.pf fmt "OFFSETCLOSURE %d" n
  | GETGLOBAL n -> Fmt.pf fmt "GETGLOBAL %a" pp_data n
  | SETGLOBAL n -> Fmt.pf fmt "SETGLOBAL %a" pp_data n
  | ATOM tag -> Fmt.pf fmt "ATOM %d" tag
  | MAKEBLOCK (tag, sz) -> Fmt.pf fmt "MAKEBLOCK %d %d" tag sz
  | MAKEFLOATBLOCK sz -> Fmt.pf fmt "MAKEFLOATBLOCK %d" sz
  | GETFIELD ind -> Fmt.pf fmt "GETFIELD %d" ind
  | GETFLOATFIELD ind -> Fmt.pf fmt "GETFLOATFIELD %d" ind
  | SETFIELD ind -> Fmt.pf fmt "SETFIELD %d" ind
  | SETFLOATFIELD ind -> Fmt.pf fmt "SETFLOATFIELD %d" ind
  | GETVECTITEM -> Fmt.pf fmt "GETVECTITEM"
  | SETVECTITEM -> Fmt.pf fmt "SETVECTITEM"
  | GETBYTESCHAR -> Fmt.pf fmt "GETBYTESCHAR"
  | SETBYTESCHAR -> Fmt.pf fmt "SETBYTESCHAR"
  | GETSTRINGCHAR -> Fmt.pf fmt "GETSTRINGCHAR"
  | BRANCH ptr -> Fmt.pf fmt "BRANCH %a" pp_ptr ptr
  | BRANCHIF ptr -> Fmt.pf fmt "BRANCHIF %a" pp_ptr ptr
  | BRANCHIFNOT ptr -> Fmt.pf fmt "BRANCHIFNOT %a" pp_ptr ptr
  | SWITCH (iptrs, pptrs) ->
    Fmt.pf fmt "SWITCH %a %a" (pp_ml_array pp_ptr) iptrs (pp_ml_array pp_ptr)
      pptrs
  | PUSHTRAP ptr -> Fmt.pf fmt "PUSHTRAP %a" pp_ptr ptr
  | POPTRAP -> Fmt.pf fmt "POPTRAP"
  | RAISE -> Fmt.pf fmt "RAISE"
  | RERAISE -> Fmt.pf fmt "RERAISE"
  | RAISE_NOTRACE -> Fmt.pf fmt "RAISE_NOTRACE"
  | CHECK_SIGNALS -> Fmt.pf fmt "CHECK_SIGNALS"
  | C_CALL (narg, idx) -> Fmt.pf fmt "C_CALL %d %a" narg pp_cfun idx
  | CONSTINT n -> Fmt.pf fmt "CONSTINT %d" n
  | UNAPP unop -> Fmt.pf fmt "UNAPP %a" pp_unop unop
  | BINAPP binop -> Fmt.pf fmt "BINAPP %a" pp_binop binop
  | COMPARE compop -> Fmt.pf fmt "COMPARE %a" pp_compop compop
  | COMPBRANCH (op, n, ptr) ->
    Fmt.pf fmt "COMPBRANCH %a %d %a" pp_compop op n pp_ptr ptr
  | OFFSETREF n -> Fmt.pf fmt "OFFSETREF %d" n
  | GETMETHOD -> Fmt.pf fmt "GETMETHOD"
  | GETPUBMET tag -> Fmt.pf fmt "GETPUBMET %d" tag
  | GETDYNMET -> Fmt.pf fmt "GETDYNMET"
  | STOP -> Fmt.pf fmt "STOP"

let string_of_unop = Fmt.str "%a" pp_unop

let string_of_binop = Fmt.str "%a" pp_binop

let string_of_compop = Fmt.str "%a" pp_compop

let to_string =
  let pp_ptr fmt ptr = Fmt.pf fmt "%d" ptr in
  let pp_cfun fmt idx = Fmt.pf fmt "%d" idx in
  let pp_data fmt ind = Fmt.pf fmt "%d" ind in
  Fmt.str "%a" (pp pp_ptr pp_cfun pp_data)

(***)

let get_ptrs instr =
  match instr with
  | PUSH_RETADDR ptr
  | CLOSURE (_, ptr)
  | BRANCH ptr
  | BRANCHIF ptr
  | BRANCHIFNOT ptr
  | PUSHTRAP ptr
  | COMPBRANCH (_, _, ptr) ->
    [ ptr ]
  | CLOSUREREC (_, p) -> Array.to_list p
  | SWITCH (iptrs, pptrs) -> Array.to_list iptrs @ Array.to_list pptrs
  | _ -> []

let get_nexts ind instr =
  match instr with
  | STOP | RETURN _ | APPTERM _ | RAISE | RERAISE | RAISE_NOTRACE -> []
  | GRAB _ -> [ ind - 1; ind + 1 ]
  | BRANCH ptr -> [ ptr ]
  | BRANCHIF ptr | BRANCHIFNOT ptr | PUSH_RETADDR ptr | PUSHTRAP ptr ->
    [ ind + 1; ptr ]
  | COMPBRANCH (_, _, ptr) -> [ ind + 1; ptr ]
  | SWITCH (iptrs, pptrs) -> Array.to_list iptrs @ Array.to_list pptrs
  | _ -> [ ind + 1 ]

(***)
