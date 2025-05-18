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
  | ACC0
  | ACC1
  | ACC2
  | ACC3
  | ACC4
  | ACC5
  | ACC6
  | ACC7
  | ACC of int
  | PUSH
  | PUSHACC0
  | PUSHACC1
  | PUSHACC2
  | PUSHACC3
  | PUSHACC4
  | PUSHACC5
  | PUSHACC6
  | PUSHACC7
  | PUSHACC of int
  | POP of int
  | ASSIGN of int
  | ENVACC1
  | ENVACC2
  | ENVACC3
  | ENVACC4
  | ENVACC of int
  | PUSHENVACC1
  | PUSHENVACC2
  | PUSHENVACC3
  | PUSHENVACC4
  | PUSHENVACC of int
  | PUSH_RETADDR of int
  | APPLY of int
  | APPLY1
  | APPLY2
  | APPLY3
  | APPTERM of int * int
  | APPTERM1 of int
  | APPTERM2 of int
  | APPTERM3 of int
  | RETURN of int
  | RESTART
  | GRAB of int
  | CLOSURE of int * int
  | CLOSUREREC of int * int * int * int array
  | OFFSETCLOSUREM2
  | OFFSETCLOSURE0
  | OFFSETCLOSURE2
  | OFFSETCLOSURE of int
  | PUSHOFFSETCLOSUREM2
  | PUSHOFFSETCLOSURE0
  | PUSHOFFSETCLOSURE2
  | PUSHOFFSETCLOSURE of int
  | GETGLOBAL of int
  | PUSHGETGLOBAL of int
  | GETGLOBALFIELD of int * int
  | PUSHGETGLOBALFIELD of int * int
  | SETGLOBAL of int
  | ATOM0
  | ATOM of int
  | PUSHATOM0
  | PUSHATOM of int
  | MAKEBLOCK of int * int
  | MAKEBLOCK1 of int
  | MAKEBLOCK2 of int
  | MAKEBLOCK3 of int
  | MAKEFLOATBLOCK of int
  | GETFIELD0
  | GETFIELD1
  | GETFIELD2
  | GETFIELD3
  | GETFIELD of int
  | GETFLOATFIELD of int
  | SETFIELD0
  | SETFIELD1
  | SETFIELD2
  | SETFIELD3
  | SETFIELD of int
  | SETFLOATFIELD of int
  | VECTLENGTH
  | GETVECTITEM
  | SETVECTITEM
  | GETBYTESCHAR
  | SETBYTESCHAR
  | GETSTRINGCHAR
  | BRANCH of int
  | BRANCHIF of int
  | BRANCHIFNOT of int
  | SWITCH of int * int array
  | BOOLNOT
  | PUSHTRAP of int
  | POPTRAP
  | RAISE
  | CHECK_SIGNALS
  | C_CALL1 of int
  | C_CALL2 of int
  | C_CALL3 of int
  | C_CALL4 of int
  | C_CALL5 of int
  | C_CALLN of int * int
  | CONST0
  | CONST1
  | CONST2
  | CONST3
  | CONSTINT of int
  | PUSHCONST0
  | PUSHCONST1
  | PUSHCONST2
  | PUSHCONST3
  | PUSHCONSTINT of int
  | NEGINT
  | ADDINT
  | SUBINT
  | MULINT
  | DIVINT
  | MODINT
  | ANDINT
  | ORINT
  | XORINT
  | LSLINT
  | LSRINT
  | ASRINT
  | EQ
  | NEQ
  | LTINT
  | LEINT
  | GTINT
  | GEINT
  | OFFSETINT of int
  | OFFSETREF of int
  | ISINT
  | GETMETHOD
  | BEQ of int * int
  | BNEQ of int * int
  | BLTINT of int * int
  | BLEINT of int * int
  | BGTINT of int * int
  | BGEINT of int * int
  | ULTINT
  | UGEINT
  | BULTINT of int * int
  | BUGEINT of int * int
  | GETPUBMET of int * int
  | GETDYNMET
  | STOP
  | EVENT
  | BREAK
  | RERAISE
  | RAISE_NOTRACE

(***)

let bprint pp_ptr pp_cfun pp_data buf instr =
  let fmt = Format.formatter_of_buffer buf in
  match instr with
  | ACC0 -> Fmt.pf fmt "ACC0"
  | ACC1 -> Fmt.pf fmt "ACC1"
  | ACC2 -> Fmt.pf fmt "ACC2"
  | ACC3 -> Fmt.pf fmt "ACC3"
  | ACC4 -> Fmt.pf fmt "ACC4"
  | ACC5 -> Fmt.pf fmt "ACC5"
  | ACC6 -> Fmt.pf fmt "ACC6"
  | ACC7 -> Fmt.pf fmt "ACC7"
  | ACC n -> Fmt.pf fmt "ACC %d" n
  | PUSH -> Fmt.pf fmt "PUSH"
  | PUSHACC0 -> Fmt.pf fmt "PUSHACC0"
  | PUSHACC1 -> Fmt.pf fmt "PUSHACC1"
  | PUSHACC2 -> Fmt.pf fmt "PUSHACC2"
  | PUSHACC3 -> Fmt.pf fmt "PUSHACC3"
  | PUSHACC4 -> Fmt.pf fmt "PUSHACC4"
  | PUSHACC5 -> Fmt.pf fmt "PUSHACC5"
  | PUSHACC6 -> Fmt.pf fmt "PUSHACC6"
  | PUSHACC7 -> Fmt.pf fmt "PUSHACC7"
  | PUSHACC n -> Fmt.pf fmt "PUSHACC %d" n
  | POP n -> Fmt.pf fmt "POP %d" n
  | ASSIGN n -> Fmt.pf fmt "ASSIGN %d" n
  | ENVACC1 -> Fmt.pf fmt "ENVACC1"
  | ENVACC2 -> Fmt.pf fmt "ENVACC2"
  | ENVACC3 -> Fmt.pf fmt "ENVACC3"
  | ENVACC4 -> Fmt.pf fmt "ENVACC4"
  | ENVACC n -> Fmt.pf fmt "ENVACC %d" n
  | PUSHENVACC1 -> Fmt.pf fmt "PUSHENVACC1"
  | PUSHENVACC2 -> Fmt.pf fmt "PUSHENVACC2"
  | PUSHENVACC3 -> Fmt.pf fmt "PUSHENVACC3"
  | PUSHENVACC4 -> Fmt.pf fmt "PUSHENVACC4"
  | PUSHENVACC n -> Fmt.pf fmt "PUSHENVACC %d" n
  | PUSH_RETADDR ptr -> Fmt.pf fmt "PUSH_RETADDR %a" pp_ptr ptr
  | APPLY n -> Fmt.pf fmt "APPLY %d" n
  | APPLY1 -> Fmt.pf fmt "APPLY1"
  | APPLY2 -> Fmt.pf fmt "APPLY2"
  | APPLY3 -> Fmt.pf fmt "APPLY3"
  | APPTERM (n, s) -> Fmt.pf fmt "APPTERM %d %d" n s
  | APPTERM1 s -> Fmt.pf fmt "APPTERM1 %d" s
  | APPTERM2 s -> Fmt.pf fmt "APPTERM2 %d" s
  | APPTERM3 s -> Fmt.pf fmt "APPTERM3 %d" s
  | RETURN n -> Fmt.pf fmt "RETURN %d" n
  | RESTART -> Fmt.pf fmt "RESTART"
  | GRAB n -> Fmt.pf fmt "GRAB %d" n
  | CLOSURE (n, ptr) -> Fmt.pf fmt "CLOSURE %d %a" n pp_ptr ptr
  | CLOSUREREC (f, v, o, t) ->
    Fmt.pf fmt "CLOSUREREC %d %d %a %a" f v pp_ptr o (Tools.pp_ml_array pp_ptr)
      t
  | OFFSETCLOSUREM2 -> Fmt.pf fmt "OFFSETCLOSUREM2"
  | OFFSETCLOSURE0 -> Fmt.pf fmt "OFFSETCLOSURE0"
  | OFFSETCLOSURE2 -> Fmt.pf fmt "OFFSETCLOSURE2"
  | OFFSETCLOSURE n -> Fmt.pf fmt "OFFSETCLOSURE %d" n
  | PUSHOFFSETCLOSUREM2 -> Fmt.pf fmt "PUSHOFFSETCLOSUREM2"
  | PUSHOFFSETCLOSURE0 -> Fmt.pf fmt "PUSHOFFSETCLOSURE0"
  | PUSHOFFSETCLOSURE2 -> Fmt.pf fmt "PUSHOFFSETCLOSURE2"
  | PUSHOFFSETCLOSURE n -> Fmt.pf fmt "PUSHOFFSETCLOSURE %d" n
  | GETGLOBAL n -> Fmt.pf fmt "GETGLOBAL %a" pp_data n
  | PUSHGETGLOBAL n -> Fmt.pf fmt "PUSHGETGLOBAL %a" pp_data n
  | GETGLOBALFIELD (n, p) -> Fmt.pf fmt "GETGLOBALFIELD %a %d" pp_data n p
  | PUSHGETGLOBALFIELD (n, p) ->
    Fmt.pf fmt "PUSHGETGLOBALFIELD %a %d" pp_data n p
  | SETGLOBAL n -> Fmt.pf fmt "SETGLOBAL %a" pp_data n
  | ATOM0 -> Fmt.pf fmt "ATOM0"
  | ATOM tag -> Fmt.pf fmt "ATOM %d" tag
  | PUSHATOM0 -> Fmt.pf fmt "PUSHATOM0"
  | PUSHATOM tag -> Fmt.pf fmt "PUSHATOM %d" tag
  | MAKEBLOCK (tag, sz) -> Fmt.pf fmt "MAKEBLOCK %d %d" tag sz
  | MAKEBLOCK1 tag -> Fmt.pf fmt "MAKEBLOCK1 %d" tag
  | MAKEBLOCK2 tag -> Fmt.pf fmt "MAKEBLOCK2 %d" tag
  | MAKEBLOCK3 tag -> Fmt.pf fmt "MAKEBLOCK3 %d" tag
  | MAKEFLOATBLOCK sz -> Fmt.pf fmt "MAKEFLOATBLOCK %d" sz
  | GETFIELD0 -> Fmt.pf fmt "GETFIELD0"
  | GETFIELD1 -> Fmt.pf fmt "GETFIELD1"
  | GETFIELD2 -> Fmt.pf fmt "GETFIELD2"
  | GETFIELD3 -> Fmt.pf fmt "GETFIELD3"
  | GETFIELD n -> Fmt.pf fmt "GETFIELD %d" n
  | GETFLOATFIELD n -> Fmt.pf fmt "GETFLOATFIELD %d" n
  | SETFIELD0 -> Fmt.pf fmt "SETFIELD0"
  | SETFIELD1 -> Fmt.pf fmt "SETFIELD1"
  | SETFIELD2 -> Fmt.pf fmt "SETFIELD2"
  | SETFIELD3 -> Fmt.pf fmt "SETFIELD3"
  | SETFIELD n -> Fmt.pf fmt "SETFIELD %d" n
  | SETFLOATFIELD n -> Fmt.pf fmt "SETFLOATFIELD %d" n
  | VECTLENGTH -> Fmt.pf fmt "VECTLENGTH"
  | GETVECTITEM -> Fmt.pf fmt "GETVECTITEM"
  | SETVECTITEM -> Fmt.pf fmt "SETVECTITEM"
  | GETBYTESCHAR -> Fmt.pf fmt "GETBYTESCHAR"
  | SETBYTESCHAR -> Fmt.pf fmt "SETBYTESCHAR"
  | GETSTRINGCHAR -> Fmt.pf fmt "GETSTRINGCHAR"
  | BRANCH ptr -> Fmt.pf fmt "BRANCH %a" pp_ptr ptr
  | BRANCHIF ptr -> Fmt.pf fmt "BRANCHIF %a" pp_ptr ptr
  | BRANCHIFNOT ptr -> Fmt.pf fmt "BRANCHIFNOT %a" pp_ptr ptr
  | SWITCH (n, ptrs) ->
    Fmt.pf fmt "SWITCH %d %a" n (Tools.pp_ml_array pp_ptr) ptrs
  | BOOLNOT -> Fmt.pf fmt "BOOLNOT"
  | PUSHTRAP ptr -> Fmt.pf fmt "PUSHTRAP %a" pp_ptr ptr
  | POPTRAP -> Fmt.pf fmt "POPTRAP"
  | RAISE -> Fmt.pf fmt "RAISE"
  | CHECK_SIGNALS -> Fmt.pf fmt "CHECK_SIGNALS"
  | C_CALL1 idx -> Fmt.pf fmt "C_CALL1 %a" pp_cfun idx
  | C_CALL2 idx -> Fmt.pf fmt "C_CALL2 %a" pp_cfun idx
  | C_CALL3 idx -> Fmt.pf fmt "C_CALL3 %a" pp_cfun idx
  | C_CALL4 idx -> Fmt.pf fmt "C_CALL4 %a" pp_cfun idx
  | C_CALL5 idx -> Fmt.pf fmt "C_CALL5 %a" pp_cfun idx
  | C_CALLN (narg, idx) -> Fmt.pf fmt "C_CALLN %d %a" narg pp_cfun idx
  | CONST0 -> Fmt.pf fmt "CONST0"
  | CONST1 -> Fmt.pf fmt "CONST1"
  | CONST2 -> Fmt.pf fmt "CONST2"
  | CONST3 -> Fmt.pf fmt "CONST3"
  | CONSTINT n -> Fmt.pf fmt "CONSTINT %d" n
  | PUSHCONST0 -> Fmt.pf fmt "PUSHCONST0"
  | PUSHCONST1 -> Fmt.pf fmt "PUSHCONST1"
  | PUSHCONST2 -> Fmt.pf fmt "PUSHCONST2"
  | PUSHCONST3 -> Fmt.pf fmt "PUSHCONST3"
  | PUSHCONSTINT n -> Fmt.pf fmt "PUSHCONSTINT %d" n
  | NEGINT -> Fmt.pf fmt "NEGINT"
  | ADDINT -> Fmt.pf fmt "ADDINT"
  | SUBINT -> Fmt.pf fmt "SUBINT"
  | MULINT -> Fmt.pf fmt "MULINT"
  | DIVINT -> Fmt.pf fmt "DIVINT"
  | MODINT -> Fmt.pf fmt "MODINT"
  | ANDINT -> Fmt.pf fmt "ANDINT"
  | ORINT -> Fmt.pf fmt "ORINT"
  | XORINT -> Fmt.pf fmt "XORINT"
  | LSLINT -> Fmt.pf fmt "LSLINT"
  | LSRINT -> Fmt.pf fmt "LSRINT"
  | ASRINT -> Fmt.pf fmt "ASRINT"
  | EQ -> Fmt.pf fmt "EQ"
  | NEQ -> Fmt.pf fmt "NEQ"
  | LTINT -> Fmt.pf fmt "LTINT"
  | LEINT -> Fmt.pf fmt "LEINT"
  | GTINT -> Fmt.pf fmt "GTINT"
  | GEINT -> Fmt.pf fmt "GEINT"
  | OFFSETINT n -> Fmt.pf fmt "OFFSETINT %d" n
  | OFFSETREF n -> Fmt.pf fmt "OFFSETREF %d" n
  | ISINT -> Fmt.pf fmt "ISINT"
  | GETMETHOD -> Fmt.pf fmt "GETMETHOD"
  | BEQ (n, ptr) -> Fmt.pf fmt "BEQ %d %a" n pp_ptr ptr
  | BNEQ (n, ptr) -> Fmt.pf fmt "BNEQ %d %a" n pp_ptr ptr
  | BLTINT (n, ptr) -> Fmt.pf fmt "BLTINT %d %a" n pp_ptr ptr
  | BLEINT (n, ptr) -> Fmt.pf fmt "BLEINT %d %a" n pp_ptr ptr
  | BGTINT (n, ptr) -> Fmt.pf fmt "BGTINT %d %a" n pp_ptr ptr
  | BGEINT (n, ptr) -> Fmt.pf fmt "BGEINT %d %a" n pp_ptr ptr
  | ULTINT -> Fmt.pf fmt "ULTINT"
  | UGEINT -> Fmt.pf fmt "UGEINT"
  | BULTINT (n, ptr) -> Fmt.pf fmt "BULTINT %d %a" n pp_ptr ptr
  | BUGEINT (n, ptr) -> Fmt.pf fmt "BUGEINT %d %a" n pp_ptr ptr
  | GETPUBMET (tag, cache) -> Fmt.pf fmt "GETPUBMET %d %d" tag cache
  | GETDYNMET -> Fmt.pf fmt "GETDYNMET"
  | STOP -> Fmt.pf fmt "STOP"
  | EVENT -> Fmt.pf fmt "EVENT"
  | BREAK -> Fmt.pf fmt "BREAK"
  | RERAISE -> Fmt.pf fmt "RERAISE"
  | RAISE_NOTRACE -> Fmt.pf fmt "RAISE_NOTRACE"

let to_string i =
  let buf = Buffer.create 64 in
  let pp_ptr fmt ptr = Fmt.pf fmt "%d" ptr in
  let pp_cfun fmt idx = Fmt.pf fmt "%d" idx in
  let pp_data fmt ind = Fmt.pf fmt "%d" ind in
  bprint pp_ptr pp_cfun pp_data buf i;
  Buffer.contents buf

(***)

let get_ptrs instr =
  match instr with
  | PUSH_RETADDR ptr
  | BRANCH ptr
  | BRANCHIF ptr
  | BRANCHIFNOT ptr
  | PUSHTRAP ptr
  | CLOSURE (_, ptr)
  | BEQ (_, ptr)
  | BNEQ (_, ptr)
  | BLTINT (_, ptr)
  | BLEINT (_, ptr)
  | BGTINT (_, ptr)
  | BGEINT (_, ptr)
  | BULTINT (_, ptr)
  | BUGEINT (_, ptr) ->
    [ ptr ]
  | CLOSUREREC (_, _, ptr, ptrs) -> ptr :: Array.to_list ptrs
  | SWITCH (_, ptrs) -> Array.to_list ptrs
  | _ -> []

let get_nexts ind instr =
  match instr with
  | STOP | RETURN _ | APPTERM _ | APPTERM1 _ | APPTERM2 _ | APPTERM3 _ | RAISE
  | RERAISE | RAISE_NOTRACE ->
    []
  | GRAB _ -> [ ind - 1; ind + 1 ]
  | BRANCH ptr -> [ ptr ]
  | BRANCHIF ptr
  | BRANCHIFNOT ptr
  | BEQ (_, ptr)
  | BNEQ (_, ptr)
  | BLTINT (_, ptr)
  | BLEINT (_, ptr)
  | BGTINT (_, ptr)
  | BGEINT (_, ptr)
  | BULTINT (_, ptr)
  | BUGEINT (_, ptr)
  | PUSH_RETADDR ptr
  | PUSHTRAP ptr ->
    [ ind + 1; ptr ]
  | SWITCH (_, ptrs) -> Array.to_list ptrs
  | _ -> [ ind + 1 ]

(***)

let read version next_word =
  let opcode =
    let w = next_word () in
    match version with
    | Version.V008 | Version.V011 | Version.V022 | Version.V023 | Version.V025
    | Version.V026 | Version.V027 | Version.V028 | Version.V029 | Version.V030
    | Version.V031 ->
      w
    | Version.V010 ->
      if w <= 91 then w
      else if w = 92 then 146
      else if w = 93 then 147
      else w - 2
  in
  match opcode with
  | 0 -> ACC0
  | 1 -> ACC1
  | 2 -> ACC2
  | 3 -> ACC3
  | 4 -> ACC4
  | 5 -> ACC5
  | 6 -> ACC6
  | 7 -> ACC7
  | 8 -> ACC (next_word ())
  | 9 -> PUSH
  | 10 -> PUSHACC0
  | 11 -> PUSHACC1
  | 12 -> PUSHACC2
  | 13 -> PUSHACC3
  | 14 -> PUSHACC4
  | 15 -> PUSHACC5
  | 16 -> PUSHACC6
  | 17 -> PUSHACC7
  | 18 -> PUSHACC (next_word ())
  | 19 -> POP (next_word ())
  | 20 -> ASSIGN (next_word ())
  | 21 -> ENVACC1
  | 22 -> ENVACC2
  | 23 -> ENVACC3
  | 24 -> ENVACC4
  | 25 -> ENVACC (next_word ())
  | 26 -> PUSHENVACC1
  | 27 -> PUSHENVACC2
  | 28 -> PUSHENVACC3
  | 29 -> PUSHENVACC4
  | 30 -> PUSHENVACC (next_word ())
  | 31 -> PUSH_RETADDR (next_word ())
  | 32 -> APPLY (next_word ())
  | 33 -> APPLY1
  | 34 -> APPLY2
  | 35 -> APPLY3
  | 36 ->
    let n = next_word () in
    let s = next_word () in
    APPTERM (n, s)
  | 37 -> APPTERM1 (next_word ())
  | 38 -> APPTERM2 (next_word ())
  | 39 -> APPTERM3 (next_word ())
  | 40 -> RETURN (next_word ())
  | 41 -> RESTART
  | 42 -> GRAB (next_word ())
  | 43 ->
    let n = next_word () in
    let ptr = next_word () in
    CLOSURE (n, ptr)
  | 44 ->
    let f = next_word () in
    let v = next_word () in
    let o = next_word () in
    let t = Array.make (f - 1) (-1) in
    for i = 0 to f - 2 do
      t.(i) <- next_word ()
    done;
    CLOSUREREC (f, v, o, t)
  | 45 -> OFFSETCLOSUREM2
  | 46 -> OFFSETCLOSURE0
  | 47 -> OFFSETCLOSURE2
  | 48 -> OFFSETCLOSURE (next_word ())
  | 49 -> PUSHOFFSETCLOSUREM2
  | 50 -> PUSHOFFSETCLOSURE0
  | 51 -> PUSHOFFSETCLOSURE2
  | 52 -> PUSHOFFSETCLOSURE (next_word ())
  | 53 -> GETGLOBAL (next_word ())
  | 54 -> PUSHGETGLOBAL (next_word ())
  | 55 ->
    let n = next_word () in
    let p = next_word () in
    GETGLOBALFIELD (n, p)
  | 56 ->
    let n = next_word () in
    let p = next_word () in
    PUSHGETGLOBALFIELD (n, p)
  | 57 -> SETGLOBAL (next_word ())
  | 58 -> ATOM0
  | 59 -> ATOM (next_word ())
  | 60 -> PUSHATOM0
  | 61 -> PUSHATOM (next_word ())
  | 62 ->
    let sz = next_word () in
    let tag = next_word () in
    MAKEBLOCK (tag, sz)
  | 63 -> MAKEBLOCK1 (next_word ())
  | 64 -> MAKEBLOCK2 (next_word ())
  | 65 -> MAKEBLOCK3 (next_word ())
  | 66 -> MAKEFLOATBLOCK (next_word ())
  | 67 -> GETFIELD0
  | 68 -> GETFIELD1
  | 69 -> GETFIELD2
  | 70 -> GETFIELD3
  | 71 -> GETFIELD (next_word ())
  | 72 -> GETFLOATFIELD (next_word ())
  | 73 -> SETFIELD0
  | 74 -> SETFIELD1
  | 75 -> SETFIELD2
  | 76 -> SETFIELD3
  | 77 -> SETFIELD (next_word ())
  | 78 -> SETFLOATFIELD (next_word ())
  | 79 -> VECTLENGTH
  | 80 -> GETVECTITEM
  | 81 -> SETVECTITEM
  | 82 -> GETBYTESCHAR
  | 83 -> SETBYTESCHAR
  | 84 -> BRANCH (next_word ())
  | 85 -> BRANCHIF (next_word ())
  | 86 -> BRANCHIFNOT (next_word ())
  | 87 ->
    let n = next_word () in
    let size_tag = n lsr 16 in
    let size_long = n land 0xFFFF in
    let size = size_tag + size_long in
    let tab = Array.init size (fun _ -> next_word ()) in
    SWITCH (n, tab)
  | 88 -> BOOLNOT
  | 89 -> PUSHTRAP (next_word ())
  | 90 -> POPTRAP
  | 91 -> RAISE
  | 92 -> CHECK_SIGNALS
  | 93 -> C_CALL1 (next_word ())
  | 94 -> C_CALL2 (next_word ())
  | 95 -> C_CALL3 (next_word ())
  | 96 -> C_CALL4 (next_word ())
  | 97 -> C_CALL5 (next_word ())
  | 98 ->
    let narg = next_word () in
    let idx = next_word () in
    C_CALLN (narg, idx)
  | 99 -> CONST0
  | 100 -> CONST1
  | 101 -> CONST2
  | 102 -> CONST3
  | 103 -> CONSTINT (next_word ())
  | 104 -> PUSHCONST0
  | 105 -> PUSHCONST1
  | 106 -> PUSHCONST2
  | 107 -> PUSHCONST3
  | 108 -> PUSHCONSTINT (next_word ())
  | 109 -> NEGINT
  | 110 -> ADDINT
  | 111 -> SUBINT
  | 112 -> MULINT
  | 113 -> DIVINT
  | 114 -> MODINT
  | 115 -> ANDINT
  | 116 -> ORINT
  | 117 -> XORINT
  | 118 -> LSLINT
  | 119 -> LSRINT
  | 120 -> ASRINT
  | 121 -> EQ
  | 122 -> NEQ
  | 123 -> LTINT
  | 124 -> LEINT
  | 125 -> GTINT
  | 126 -> GEINT
  | 127 -> OFFSETINT (next_word ())
  | 128 -> OFFSETREF (next_word ())
  | 129 -> ISINT
  | 130 -> GETMETHOD
  | 131 ->
    let n = next_word () in
    let ptr = next_word () in
    BEQ (n, ptr)
  | 132 ->
    let n = next_word () in
    let ptr = next_word () in
    BNEQ (n, ptr)
  | 133 ->
    let n = next_word () in
    let ptr = next_word () in
    BLTINT (n, ptr)
  | 134 ->
    let n = next_word () in
    let ptr = next_word () in
    BLEINT (n, ptr)
  | 135 ->
    let n = next_word () in
    let ptr = next_word () in
    BGTINT (n, ptr)
  | 136 ->
    let n = next_word () in
    let ptr = next_word () in
    BGEINT (n, ptr)
  | 137 -> ULTINT
  | 138 -> UGEINT
  | 139 ->
    let n = next_word () in
    let ptr = next_word () in
    BULTINT (n, ptr)
  | 140 ->
    let n = next_word () in
    let ptr = next_word () in
    BUGEINT (n, ptr)
  | 141 ->
    let tag = next_word () in
    let cache = next_word () in
    GETPUBMET (tag, cache)
  | 142 -> GETDYNMET
  | 143 -> STOP
  | 144 -> EVENT
  | 145 -> BREAK
  | 146 -> RERAISE
  | 147 -> RAISE_NOTRACE
  | 148 -> GETSTRINGCHAR
  | _ -> Fmt.failwith "invalid opcode: %d" opcode

let write version write_word write_ptr instr =
  let write_opcode w =
    match (version, w) with
    | Version.V008, (146 | 147) -> write_word 91
    | Version.V008, 148 -> write_word 82
    | Version.V008, _ -> write_word w
    | Version.V010, 146 -> write_word 92
    | Version.V010, 147 -> write_word 93
    | Version.V010, 148 -> write_word 82
    | Version.V010, _ -> write_word (if w <= 91 then w else w + 2)
    | Version.V011, 148 -> write_word 82
    | Version.V011, _ -> write_word w
    | Version.V022, _
    | Version.V023, _
    | Version.V025, _
    | Version.V026, _
    | Version.V027, _
    | Version.V028, _
    | Version.V029, _
    | Version.V030, _
    | Version.V031, _ ->
      write_word w
  in
  let write_ptrs delta ptrs = Array.iter (write_ptr delta) ptrs in
  match instr with
  | ACC0 -> write_opcode 0
  | ACC1 -> write_opcode 1
  | ACC2 -> write_opcode 2
  | ACC3 -> write_opcode 3
  | ACC4 -> write_opcode 4
  | ACC5 -> write_opcode 5
  | ACC6 -> write_opcode 6
  | ACC7 -> write_opcode 7
  | ACC n ->
    write_opcode 8;
    write_word n
  | PUSH -> write_opcode 9
  | PUSHACC0 -> write_opcode 10
  | PUSHACC1 -> write_opcode 11
  | PUSHACC2 -> write_opcode 12
  | PUSHACC3 -> write_opcode 13
  | PUSHACC4 -> write_opcode 14
  | PUSHACC5 -> write_opcode 15
  | PUSHACC6 -> write_opcode 16
  | PUSHACC7 -> write_opcode 17
  | PUSHACC n ->
    write_opcode 18;
    write_word n
  | POP n ->
    write_opcode 19;
    write_word n
  | ASSIGN n ->
    write_opcode 20;
    write_word n
  | ENVACC1 -> write_opcode 21
  | ENVACC2 -> write_opcode 22
  | ENVACC3 -> write_opcode 23
  | ENVACC4 -> write_opcode 24
  | ENVACC n ->
    write_opcode 25;
    write_word n
  | PUSHENVACC1 -> write_opcode 26
  | PUSHENVACC2 -> write_opcode 27
  | PUSHENVACC3 -> write_opcode 28
  | PUSHENVACC4 -> write_opcode 29
  | PUSHENVACC n ->
    write_opcode 30;
    write_word n
  | PUSH_RETADDR ptr ->
    write_opcode 31;
    write_ptr 1 ptr
  | APPLY n ->
    write_opcode 32;
    write_word n
  | APPLY1 -> write_opcode 33
  | APPLY2 -> write_opcode 34
  | APPLY3 -> write_opcode 35
  | APPTERM (n, s) ->
    write_opcode 36;
    write_word n;
    write_word s
  | APPTERM1 s ->
    write_opcode 37;
    write_word s
  | APPTERM2 s ->
    write_opcode 38;
    write_word s
  | APPTERM3 s ->
    write_opcode 39;
    write_word s
  | RETURN n ->
    write_opcode 40;
    write_word n
  | RESTART -> write_opcode 41
  | GRAB n ->
    write_opcode 42;
    write_word n
  | CLOSURE (n, ptr) ->
    write_opcode 43;
    write_word n;
    write_ptr 2 ptr
  | CLOSUREREC (f, v, o, t) ->
    write_opcode 44;
    write_word f;
    write_word v;
    write_ptr 3 o;
    write_ptrs 3 t
  | OFFSETCLOSUREM2 -> write_opcode 45
  | OFFSETCLOSURE0 -> write_opcode 46
  | OFFSETCLOSURE2 -> write_opcode 47
  | OFFSETCLOSURE n ->
    write_opcode 48;
    write_word n
  | PUSHOFFSETCLOSUREM2 -> write_opcode 49
  | PUSHOFFSETCLOSURE0 -> write_opcode 50
  | PUSHOFFSETCLOSURE2 -> write_opcode 51
  | PUSHOFFSETCLOSURE n ->
    write_opcode 52;
    write_word n
  | GETGLOBAL n ->
    write_opcode 53;
    write_word n
  | PUSHGETGLOBAL n ->
    write_opcode 54;
    write_word n
  | GETGLOBALFIELD (n, p) ->
    write_opcode 55;
    write_word n;
    write_word p
  | PUSHGETGLOBALFIELD (n, p) ->
    write_opcode 56;
    write_word n;
    write_word p
  | SETGLOBAL n ->
    write_opcode 57;
    write_word n
  | ATOM0 -> write_opcode 58
  | ATOM tag ->
    write_opcode 59;
    write_word tag
  | PUSHATOM0 -> write_opcode 60
  | PUSHATOM tag ->
    write_opcode 61;
    write_word tag
  | MAKEBLOCK (tag, sz) ->
    write_opcode 62;
    write_word sz;
    write_word tag
  | MAKEBLOCK1 tag ->
    write_opcode 63;
    write_word tag
  | MAKEBLOCK2 tag ->
    write_opcode 64;
    write_word tag
  | MAKEBLOCK3 tag ->
    write_opcode 65;
    write_word tag
  | MAKEFLOATBLOCK sz ->
    write_opcode 66;
    write_word sz
  | GETFIELD0 -> write_opcode 67
  | GETFIELD1 -> write_opcode 68
  | GETFIELD2 -> write_opcode 69
  | GETFIELD3 -> write_opcode 70
  | GETFIELD n ->
    write_opcode 71;
    write_word n
  | GETFLOATFIELD n ->
    write_opcode 72;
    write_word n
  | SETFIELD0 -> write_opcode 73
  | SETFIELD1 -> write_opcode 74
  | SETFIELD2 -> write_opcode 75
  | SETFIELD3 -> write_opcode 76
  | SETFIELD n ->
    write_opcode 77;
    write_word n
  | SETFLOATFIELD n ->
    write_opcode 78;
    write_word n
  | VECTLENGTH -> write_opcode 79
  | GETVECTITEM -> write_opcode 80
  | SETVECTITEM -> write_opcode 81
  | GETBYTESCHAR -> write_opcode 82
  | SETBYTESCHAR -> write_opcode 83
  | BRANCH ptr ->
    write_opcode 84;
    write_ptr 1 ptr
  | BRANCHIF ptr ->
    write_opcode 85;
    write_ptr 1 ptr
  | BRANCHIFNOT ptr ->
    write_opcode 86;
    write_ptr 1 ptr
  | SWITCH (n, ptrs) ->
    write_opcode 87;
    write_word n;
    write_ptrs 2 ptrs
  | BOOLNOT -> write_opcode 88
  | PUSHTRAP ptr ->
    write_opcode 89;
    write_ptr 1 ptr
  | POPTRAP -> write_opcode 90
  | RAISE -> write_opcode 91
  | CHECK_SIGNALS -> write_opcode 92
  | C_CALL1 idx ->
    write_opcode 93;
    write_word idx
  | C_CALL2 idx ->
    write_opcode 94;
    write_word idx
  | C_CALL3 idx ->
    write_opcode 95;
    write_word idx
  | C_CALL4 idx ->
    write_opcode 96;
    write_word idx
  | C_CALL5 idx ->
    write_opcode 97;
    write_word idx
  | C_CALLN (narg, idx) ->
    write_opcode 98;
    write_word narg;
    write_word idx
  | CONST0 -> write_opcode 99
  | CONST1 -> write_opcode 100
  | CONST2 -> write_opcode 101
  | CONST3 -> write_opcode 102
  | CONSTINT n ->
    write_opcode 103;
    write_word n
  | PUSHCONST0 -> write_opcode 104
  | PUSHCONST1 -> write_opcode 105
  | PUSHCONST2 -> write_opcode 106
  | PUSHCONST3 -> write_opcode 107
  | PUSHCONSTINT n ->
    write_opcode 108;
    write_word n
  | NEGINT -> write_opcode 109
  | ADDINT -> write_opcode 110
  | SUBINT -> write_opcode 111
  | MULINT -> write_opcode 112
  | DIVINT -> write_opcode 113
  | MODINT -> write_opcode 114
  | ANDINT -> write_opcode 115
  | ORINT -> write_opcode 116
  | XORINT -> write_opcode 117
  | LSLINT -> write_opcode 118
  | LSRINT -> write_opcode 119
  | ASRINT -> write_opcode 120
  | EQ -> write_opcode 121
  | NEQ -> write_opcode 122
  | LTINT -> write_opcode 123
  | LEINT -> write_opcode 124
  | GTINT -> write_opcode 125
  | GEINT -> write_opcode 126
  | OFFSETINT n ->
    write_opcode 127;
    write_word n
  | OFFSETREF n ->
    write_opcode 128;
    write_word n
  | ISINT -> write_opcode 129
  | GETMETHOD -> write_opcode 130
  | BEQ (n, ptr) ->
    write_opcode 131;
    write_word n;
    write_ptr 2 ptr
  | BNEQ (n, ptr) ->
    write_opcode 132;
    write_word n;
    write_ptr 2 ptr
  | BLTINT (n, ptr) ->
    write_opcode 133;
    write_word n;
    write_ptr 2 ptr
  | BLEINT (n, ptr) ->
    write_opcode 134;
    write_word n;
    write_ptr 2 ptr
  | BGTINT (n, ptr) ->
    write_opcode 135;
    write_word n;
    write_ptr 2 ptr
  | BGEINT (n, ptr) ->
    write_opcode 136;
    write_word n;
    write_ptr 2 ptr
  | ULTINT -> write_opcode 137
  | UGEINT -> write_opcode 138
  | BULTINT (n, ptr) ->
    write_opcode 139;
    write_word n;
    write_ptr 2 ptr
  | BUGEINT (n, ptr) ->
    write_opcode 140;
    write_word n;
    write_ptr 2 ptr
  | GETPUBMET (tag, cache) ->
    write_opcode 141;
    write_word tag;
    write_word cache
  | GETDYNMET -> write_opcode 142
  | STOP -> write_opcode 143
  | EVENT -> write_opcode 144
  | BREAK -> write_opcode 145
  | RERAISE -> write_opcode 146
  | RAISE_NOTRACE -> write_opcode 147
  | GETSTRINGCHAR -> write_opcode 148
