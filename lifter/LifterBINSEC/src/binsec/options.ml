(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)


(* Disassembly options *)
let opaque_predicates_file = ref ""
let violated_call_ret_file = ref ""

(* Global vars not assigned by command line
 * FIXME: Remove them!
 *)
let finalsize = ref 0
let initsize = ref 0
let itemps = ref 0
let iflags = ref 0
let ftemps = ref 0
let fflags = ref 0
(* --------------- *)


let get_entry_point = (fun () -> None)
let flatten_memory = ref false

(* Simulation options *)
module SemanticsMode = struct
  let name = "mem_mode"
  let doc = "Set semantic memory model"

  let models =  ["flat";"region";"region-load-store";"rewrite";"logic"]

  let flat = ref false
  let non_symb_base = ref false
  let non_symb_smt = ref false
  let non_symb_base_affine = ref false

  let set s =
    match String.lowercase s with
    | "flat" -> flat := true
    | "region" ->
      non_symb_base := true;
      non_symb_smt := true
    | "region-load-store" ->
      non_symb_smt := true;
      non_symb_base_affine := true
    | "rewrite" ->
      non_symb_smt := true
    | "logic" ->
      non_symb_base := true
    | _ -> assert false

  let arg = "-"^name, Arg.Symbol(models, set), " "^doc

  let flat_or_not_basic () = !flat || !non_symb_base
  let flat_or_basic_and_full () = !flat || !non_symb_base && !non_symb_smt
  let basic () = not !non_symb_base
  let basic_affine () = not !non_symb_base_affine
  let flat () = !flat

  let to_string ()
    =
    let no_basic = !non_symb_base
    and no_full = !non_symb_smt
    and flat = flat () in
    if flat then "flat"
    else if no_basic && no_full then ""
    else if no_basic then "full"
    else if no_full then "basic"
    else "basic_full"

end
