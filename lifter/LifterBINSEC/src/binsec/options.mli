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


val get_entry_point : unit -> Bigint.t option

(** Opaque predicate information file (for assisted disassembly) *)
val opaque_predicates_file: string ref

(** Call stack tampering information file (for assisted disassembly *)
val violated_call_ret_file: string ref

(** {3 Others stats global vars} *)

val finalsize : int ref
val initsize : int ref
val ftemps: int ref
val itemps: int ref
val fflags: int ref
val iflags: int ref

(** Remove the array theory from formula generated. {b Warning:
    only works when providing a full concrete memory addressing
    as concretization policy} *)
val flatten_memory: bool ref

(** Simulation options *)
module SemanticsMode : sig
  val to_string : unit -> string
  val arg : string * Arg.spec * string

  val flat_or_not_basic : unit -> bool
  val flat_or_basic_and_full : unit -> bool
  val basic : unit -> bool
  val basic_affine : unit -> bool
  val flat : unit -> bool
end
