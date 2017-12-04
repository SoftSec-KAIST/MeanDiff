module MeanDiff.StreamGen

open CmdOpt
open Utils

let xedPath = "./build/ExternalXED"

let addBefore pf lst = List.map (fun elt -> pf + elt) lst

let mixTwoLists lst1 lst2 =
  List.fold
      (fun lst elt -> lst @ (addBefore elt lst2))
      []
      lst1

let mixThreeLists lst1 lst2 lst3 =
  mixTwoLists lst2 lst3 |> mixTwoLists lst1

let mixFourLists lst1 lst2 lst3 lst4 =
  lst4
  |> mixTwoLists lst3
  |> mixTwoLists lst2
  |> mixTwoLists lst1

// Prefix Group 1
let prefixCandidateType1 = ["" ; "f0" ; "f2" ; "f3"]

// Prefix Group 2
let prefixCandidateType2 = ["" ; "64"]

// Prefix Group 3
let prefixCandidateType3 = ["" ; "66"]

// Prefix Group 4
let prefixCandidateType4 = ["" ; "67"]

let prefixFilterList32 =
  ["f0" ; "f2" ; "f3" ; "2e" ; "36" ; "3e" ; "26" ; "64" ; "65" ; "66" ; "67" ;
    "0f" ; "c4" ; "c5"]

let prefixFilterList64 =
  seq [prefixFilterList32 ; List.map toHexStr [0x40 .. 0x4f]] |> List.concat

let twoBytesFilterList = ["38" ; "3a"]

// Only ModRM
let modrmCandidateType1 = ["00" ; "08" ; "c0" ; "c8"]

// ModRM + SIB
let modrmCandidateType2 = ["04"]

// ModRM + disp8
let modrmCandidateType3 = ["40" ; "48"]

// ModRM + disp32
let modrmCandidateType4 = ["05" ; "0d" ; "80" ; "88"]

// ModRM + SIB + disp8
let modrmCandidateType5 = ["44" ; "4c"]

// ModRM + SIB + disp32
let modrmCandidateType6 = ["84" ; "8c"]

// Only SIB
let sibCandidateType1 = ["00" ; "01" ; "20" ; "21" ; "40" ; "41"]

// SIB + disp32
let sibCandidateType2 = ["05" ; "25"]

// constant8
let conCandidateType1 = ["00" ; "42" ; "ff"]

// constant32
let conCandidateType2 = ["00000000" ; "42424242" ; "ffffffff"]

// constant64
let conCandidateType3 =
  ["0000000000000000" ; "4242424242424242" ; "ffffffffffffffff"]

let genPrefixCandidates arch =
  let basePrefix =
    mixFourLists
        prefixCandidateType1
        prefixCandidateType2
        prefixCandidateType3
        prefixCandidateType4
  match arch with
  | X86 -> basePrefix
  | X64 -> List.fold (fun l x -> x :: x + "48" :: l) [] basePrefix

let rexPrefixCandidate = ["48"]

let vexPrefixCandidate =
  List.fold (fun l x -> x :: (0b10000000 ||| x) :: l) [] [0 .. 31]
  |> List.map toHexStr
  |> mixTwoLists (List.map (fun x -> toHexStr (0b11100000 ||| x)) [0 .. 31])
  |> addBefore "c4"

let oneByteOpcodes32 =
  List.map toHexStr [0 .. 255]
  |> List.filter (fun x -> List.contains x prefixFilterList32 |> not)

let oneByteOpcodes64 =
  List.map toHexStr [0 .. 255]
  |> List.filter (fun x -> List.contains x prefixFilterList64 |> not)

let twoBytesOpcodes =
  List.map toHexStr [0 .. 255]
  |> List.filter (fun x -> List.contains x twoBytesFilterList |> not)
  |> addBefore "0f"

let threeBytesOpcodes =
  List.map toHexStr [0 .. 255]
  |> List.fold (fun l x -> "38" + x :: "3a" + x :: l) []
  |> addBefore "0f"

let opcodeCandidates32 =
  seq [oneByteOpcodes32 ; twoBytesOpcodes ; threeBytesOpcodes] |> List.concat

let opcodeCandidates64 =
  seq [oneByteOpcodes64 ; twoBytesOpcodes ; threeBytesOpcodes] |> List.concat

let modrmCandidates =
  seq [
    modrmCandidateType1 ;
    mixTwoLists modrmCandidateType2 sibCandidateType1 ;
    mixThreeLists modrmCandidateType2 sibCandidateType2 conCandidateType2 ;
    mixTwoLists modrmCandidateType3 conCandidateType1 ;
    mixTwoLists modrmCandidateType4 conCandidateType2 ;
    mixThreeLists modrmCandidateType5 sibCandidateType1 conCandidateType1 ;
    mixThreeLists modrmCandidateType6 sibCandidateType1 conCandidateType2
  ] |> List.concat

let addOperandByType opcode = function
  | 0 ->
      [opcode]
  | 1 ->
      modrmCandidates |> addBefore opcode
  | 2 ->
      conCandidateType1 |> addBefore opcode
  | 3 ->
      conCandidateType2 |> addBefore opcode
  | 4 ->
      conCandidateType3 |> addBefore opcode
  | 5 ->
        conCandidateType1 |> mixTwoLists modrmCandidates |> addBefore opcode
  | 6 ->
        conCandidateType2 |> mixTwoLists modrmCandidates |> addBefore opcode
  | 7 ->
        conCandidateType3 |> mixTwoLists modrmCandidates |> addBefore opcode
  | _ -> []

let getType arch opcode =
  let size = (String.length opcode) / 2
  let archStr =
    match arch with
    | X86 -> "x86"
    | X64 -> "x64"
  let arg = sprintf "-i %s -s %d -a %s -m gettype" opcode size archStr
  let args = [xedPath ; arg]
  procRetValue args

let checkInsn arch insn =
  let size = (String.length insn) / 2
  let archStr =
    match arch with
    | X86 -> "x86"
    | X64 -> "x64"
  let arg = sprintf "-i %s -s %d -a %s -m checkinsn" insn size archStr
  let args =[xedPath ; arg]
  procRetValue args

let allGen arch =
  match arch with
  | X86 ->
      opcodeCandidates32
      |> List.fold
          (fun pool opcode ->
              seq [
                getType arch opcode
                |> addOperandByType opcode
                |> mixTwoLists (genPrefixCandidates arch) ;
                pool
              ] |> List.concat)
          []
      |> List.filter (fun insn -> checkInsn arch insn = 1)
  | X64 ->
      opcodeCandidates64
      |> List.fold
          (fun pool opcode ->
              seq [
                getType arch opcode
                |> addOperandByType opcode
                |> mixTwoLists (genPrefixCandidates arch) ;
                pool
              ] |> List.concat)
          []
      |> List.filter (fun insn -> checkInsn arch insn = 1)

let blackListGen arch path =
  let target = readLinesToList path
  match arch with
  | X86 ->
      opcodeCandidates32
      |> List.filter (fun x -> List.contains x target |> not)
      |> List.fold
          (fun pool opcode ->
              seq [
                getType arch opcode
                |> addOperandByType opcode
                |> mixTwoLists (genPrefixCandidates arch) ;
                pool
              ] |> List.concat)
          []
      |> List.filter (fun insn -> checkInsn arch insn = 1)
  | X64 ->
      opcodeCandidates64
      |> List.filter (fun x -> List.contains x target |> not)
      |> List.fold
          (fun pool opcode ->
              seq [
                getType arch opcode
                |> addOperandByType opcode
                |> mixTwoLists (genPrefixCandidates arch) ;
                pool
              ] |> List.concat)
          []
      |> List.filter (fun insn -> checkInsn arch insn = 1)

let whiteListGen arch path =
  readLinesToList path
  |> List.fold
      (fun pool opcode ->
          seq [
            getType arch opcode
            |> addOperandByType opcode
            |> mixTwoLists (genPrefixCandidates arch) ;
            pool
          ] |> List.concat)
      []
  |> List.filter (fun insn -> checkInsn arch insn = 1)

let loadInsn (arch : ArchOption) =
  arch.toString
  |> sprintf "insn_%s"
  |> readLinesToList

let saveInsn (arch : ArchOption) insnList =
  arch.toString
  |> sprintf "insn_%s"
  |> writeListToFile insnList

let streamGen options =
  let arch = options.archOption
  match options.streamOption with
  | ALL -> allGen arch |> saveInsn arch ; []
  | BLACK -> blackListGen arch options.streamFile |> saveInsn arch ; []
  | WHITE -> whiteListGen arch options.streamFile |> saveInsn arch ; []
  | USE -> loadInsn options.archOption
