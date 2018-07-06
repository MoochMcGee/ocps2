type register =
    | R0 (* always zero *)
    | R1  | R2  | R3  | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
    | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31
    [@@deriving enum]

type opcode =
    (* 0x00 *)
    | Mips_ALU   (* MIPS splits arithmetic instructions into an ALU block
                  * then uses the `function` section at the end of the
                  * instruction to disambiguate.
                  *)
    (* 0x01 is unused *)
    | Mips_J     (* Jump *) [@value 0x02]
    | Mips_JAL   (* Jump and Link *)
    | Mips_BEQ   (* Branch if Equal *)
    | Mips_BNE   (* Branch if Not Equal *)
    | Mips_BLEZ  (* Branch if Less Than or Equal to Zero *)
    (* 0x07 is unused *)
    | Mips_ADDI  (* Add Immediate *) [@value 0x08]
    | Mips_ADDIU (* Add Unsigned Immediate *)
    | Mips_SLTI  (* Set to 1 if Less Than Immediate *)
    | Mips_SLTIU (* Set to 1 if Less Than Unsigned Immediate *)
    | Mips_ANDI  (* Bitwise AND Immediate *)
    | Mips_ORI   (* Bitwise OR Immediate *)
    (* 0x0E is unused *)
    | Mips_LUI   (* Load Upper Immediate *) [@value 0x0F]
    | Mips_MFC0  (* Move from Coprocessor 0 *)
    (* 0x11 - 0x22 are unused *)
    | Mips_LW    (* Load Word *) [@value 0x23]
    | Mips_LBU   (* Load Byte Unsigned *)
    | Mips_LHU   (* Load Halfword Unsigned *)
    (* 0x26 - 0x27 are unused *)
    | Mips_SB    (* Store Byte *) [@value 0x28]
    | Mips_SH    (* Store Halfword *)
    (* 0x2A is unused *)
    | Mips_SW    (* Store Word *) [@value 0x2B]
    [@@deriving enum]

(* integer: register and register to register
 * OP rd, rs, rt
 * encoded as
 * 31   25             20              15     10      5              0
 * | op | first source | second source | dest | shift | ALU function |
 *)
type inst_r = {
    op    : opcode;
    rs    : register;
    rt    : register;
    shift : int;
    funct : int;
}

type opcode_type =
    | Optype_R  (* integer: register and register to register *)
    | Optype_I  (* integer: register and immediate to register *)
    | Optype_J  (* jump *)
    (* Not yet implemented:
    | Optype_FR (* float: register and register to register *)
    | Optype_FI (* float: register and immediate to register *)
    *)

type t =
    | Inst_R of inst_r
    (*| Inst_I of inst_i*)
    (*| Inst_J of inst_j*)
    | NYI

let type_of_opcode = function
    | Mips_ALU  | Mips_MFC0 -> Optype_R
    | Mips_J    | Mips_JAL  -> Optype_J
    | Mips_BEQ  | Mips_BNE   | Mips_BLEZ | Mips_ADDI | Mips_ADDIU
    | Mips_SLTI | Mips_SLTIU | Mips_ANDI | Mips_ORI  | Mips_LUI
    | Mips_LW   | Mips_LBU   | Mips_LHU  | Mips_SB   | Mips_SH
    | Mips_SW  -> Optype_I

let decode s =
    assert (String.length s = 4);
    let open Stdint.Uint32 in
    let (>>) = shift_right_logical in
    let inst = of_bytes_little_endian s in
    let op = inst >> 26 in
    match op |> to_int |> opcode_of_enum |> type_of_opcode with
    (*| Optype_R -> OpcodeR.decode inst*)
    (*| Optype_I -> OpcodeI.decode inst*)
    (*| Optype_J -> OpcodeJ.decode inst*)
    | _ -> failwith "Opcodes with formats other than R are not yet implemented"
