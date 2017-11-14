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

open Dba
open Errors
open Format



let _opaque_predicates_map = ref Dba_types.Caddress.Map.empty
let violated_calls = ref Dba_types.Caddress.Set.empty
let _regular_calls = ref Dba_types.Caddress.Set.empty

let _check_opaque_predicates addr opaque_predicates_map nextaddrs =
  try
    let next_addr = Dba_types.Caddress.Map.find addr opaque_predicates_map in
    match next_addr with
    | None -> nextaddrs
    | Some a -> [a]
  with Not_found -> nextaddrs

module Program = struct
  type t = {
    instructions : Disasm_types.Program.t;
    callsites    : Dba_types.Caddress.Set.t
  }

  let empty = {
    instructions = Dba_types.Virtual_address.Map.empty;
    callsites = Dba_types.Caddress.Set.empty;
  }

  let create instructions callsites = { instructions; callsites; }

  let on_instructions f p = { p with instructions = f p.instructions }

  let on_callsites f p = { p with callsites = f p.callsites }

  let is_callsite caddr p = Dba_types.Caddress.Set.mem caddr p.callsites

  let add_callsite p callsite =
    on_callsites (Dba_types.Caddress.Set.add callsite) p

  let add_callsites = List.fold_left add_callsite

  let ppf_tag_functions ppf =
    let print_open_tag _ = ()
    and print_close_tag = function
      | "function" -> fprintf ppf "@;<8 0> ; <_fun>"
      | _ -> ()
    in
    let mark_open_tag = function
      | "function" ->
        if Logger.get_color () then "\027[0;36m" else ""
      | _ -> ""
    and mark_close_tag = function
      | "function" ->
        if Logger.get_color () then "\027[0m" else ""
      | _ -> ""
    in { mark_open_tag;  mark_close_tag;
         print_open_tag; print_close_tag; }


  let pp_no_dba vaddr ppf ginstr =
    let binstr = Disasm_types.Instruction.to_generic_instruction ginstr in
    let opcode_str =
      asprintf "%a" Disasm_types.GenericInstruction.pp_opcode binstr
      |> String.trim
    in
    (* The X86 standard says in 2.3.11:
       - The maximum length of an Intel 64 and IA-32 instruction
       remains 15 bytes.
       Assuming the opcode is made of groups of 2 nibbles (1 byte),
       separated by 1 space, the max string length is computed to be:
       2 * 15 + 15 / 2  = 38

       Adjust (upwards) the value whenever other assembly languages
       have higher requirements.
     *)

    fprintf ppf "%a@ %-38s@ %a"
            Dba_types.Virtual_address.pp vaddr
            opcode_str
            Disasm_types.GenericInstruction.pp_mnemonic binstr

  let pp ppf p =
    let open Dba_types.Virtual_address in
    pp_set_formatter_tag_functions ppf (ppf_tag_functions ppf);
    pp_set_mark_tags ppf true;
    pp_set_print_tags ppf true;
    fprintf ppf "@[<v 0>";
    Map.iter
      (fun vaddr ginstr ->
        let tag_string =
          if is_callsite (to_code_address vaddr) p then "function" else "" in
        fprintf ppf "@[<h>@{<%s>%a@}@]@ " tag_string (pp_no_dba vaddr) ginstr)
      p.instructions;
    fprintf ppf "@]";
    pp_set_mark_tags ppf false;
    pp_set_print_tags ppf false

end


open Program


(* Other functionalities *)
let custom_pp_dbainstrs opc ppf dba_block =
  let open Dba_printer.EICUnicode in
  let open Dba_types in
  let spaces = String.make (String.length opc) ' ' in
  pp_set_margin ppf 250;
  fprintf ppf "@[";
  let mypp i ppf instr = fprintf ppf "@[<h>%2d: %a@]" i pp_instruction instr in
  begin
    match Dba_types.Block.length dba_block with
    | 0 -> ()
    | 1 ->
      fprintf ppf "@[<h>%s → %a@]"
        opc
        pp_instruction (Block.get dba_block 0)
    | 2 ->
      let dbainstr1 = Block.get dba_block 0
      and dbainstr2 = Block.get dba_block 1 in
      fprintf ppf "@[<v 0> %s ⎧1: %a@  %s ⎩2: %a@ @]"
        opc
        pp_instruction dbainstr1 spaces
        pp_instruction dbainstr2

    | nelts ->
      let middle = nelts / 2 in
      let pp_bar fmt i =
        if i = middle then fprintf fmt "%s ⎨" opc
        else fprintf fmt "%s ⎪" spaces
      in
      let rec aux i =
        let e = Block.get dba_block i in
        if i = 0 then begin
          fprintf ppf "@[<v 0>@[<h>%s ⎧%a@]@ " spaces (mypp i) e;
          aux 1
        end
        else if i = nelts - 1 then
          fprintf ppf "@[<h>%s ⎩%a@]@]" spaces (mypp i) e
        else begin
          fprintf ppf "@[<h>%a%a@]@ " pp_bar i (mypp i) e;
          aux (i + 1)
        end
      in aux 0
  end;
  fprintf ppf "@]@."

let decode raw =
  try
    let opc, dba_block = Decode_utils.decode_hex_opcode raw in
    Logger.result "%a" (custom_pp_dbainstrs opc) dba_block;

  with X86toDba.InstructionUnhandled s ->
    Logger.warning "Not decoded %s" s;

