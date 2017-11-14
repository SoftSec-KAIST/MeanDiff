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

(** Disassemblers & utilities *)

module Program : sig
  type t = private {
    instructions : Disasm_types.Program.t;
    callsites    : Dba_types.Caddress.Set.t
  }

  val empty : t
  val create : Disasm_types.Program.t -> Dba_types.Caddress.Set.t  -> t
  val pp : Format.formatter -> t -> unit
end

val decode : string -> unit
(** [decode s] decodes the string opcode [s].
    @assumes [s] is an hexadecimal string, i.e. of the form [0-9a-f]+
 *)
