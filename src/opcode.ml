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

(* ALU function field; only on R type instructions *)
type funct =
    | Funct_SLL     (* Shift-left logical *)
    (* 0x01 is MOVCI *)
    | Funct_SRL     (* Shift-right logical *) [@value 0x02]
    | Funct_SRA     (* Shift-right arithmetic *)
    | Funct_SLLV    (* Shift-left logical by variable *)
    (* 0x05 is reserved *)
    | Funct_SRLV    (* Shift-right logical by variable *) [@value 0x06]
    | Funct_SRAV    (* Shift-right arithmetic by variable *)
    | Funct_JR      (* Jump register *)
    | Funct_JALR    (* Jump and link register *)
    (* 0x0A is MIPS IV MOVZ
     * 0x0B is MIPS IV MOVN
     *)
    | Funct_SYSCALL (* System call *) [@value 0x0C]
    | Funct_BREAK   (* Return from system call *)
    (* 0x0E is reserved
     * 0x0F is SYNC
     *)
    | Funct_MFHI    (* Move from HI *) [@value 0x10]
    | Funct_MTHI    (* Move to HI *)
    | Funct_MFLO    (* Move from LO *)
    | Funct_MTLO    (* Move to LO *)
    (* MIPS III - TODO
    | Funct_DSLLV   (* 64-bit shift-left logical by variable *)
    (* 0x15 is unused *)
    | Funct_DSRLV   (* 64-bit shift-right logical by variable *) [@value 0x16]
    | Funct_DSRAV   (* 64-bit shift-right arithmetic by variable *)
    *)
    | Funct_MULT    (* Multiply *) [@value 0x18]
    | Funct_MULTU   (* Multiply unsigned *)
    | Funct_DIV     (* Divide *)
    | Funct_DIVU    (* Divide unsigned *)
    (* MIPS III 64-bit arithmetic - TODO
    | Funct_DMULT   (* 64-bit multiply *)
    | Funct_DMULTU  (* 64-bit multiply unsigned *)
    | Funct_DDIV    (* 64-bit divide *)
    | Funct_DDIVU   (* 64-bit divide unsigned *)
    *)
    | Funct_ADD     (* Addition *) [@value 0x20]
    | Funct_ADDU    (* Unsigned addition *)
    | Funct_SUB     (* Subtraction *)
    | Funct_SUBU    (* Unsigned subtraction *)
    | Funct_AND     (* Logical AND *)
    | Funct_OR      (* Logical OR *)
    | Funct_XOR     (* Logical XOR *)
    | Funct_NOR     (* Logical NOR *)
    (* 0x28 and 0x29 are reserved *)
    | Funct_SLT     (* Set if less than (signed) *) [@value 0x2A]
    | Funct_SLTU    (* Set if less than (unsigned) *)
    | Funct_DADD    (* 64-bit addition *)
    | Funct_DADDU   (* 64-bit unsigned addition *)
    (* MIPS III 64-bit arithmetic - TODO
    | Funct_DSUB    (* 64-bit subtraction *)
    | Funct_DSUBU   (* 64-bit unsigned subtraction *)
    *)
    (* MIPS II conditional traps - TODO
    | Funct_TGE     (* Trap if greater than *)
    | Funct_TGEU    (* Trap if greater than (unsigned) *)
    | Funct_TLT     (* Trap if less than *)
    | Funct_TLTU    (* Trap if less than (unsigned) *)
    | Funct_TEQ     (* Trap if equal *)
    (* 0x35 is reserved *)
    | Funct_TNE     (* Trap if not equal *) [@value 0x36]
    (* 0x37 is reserved *)
    *)
    (* MIPS III 64-bit arithmetic - TODO
    | Funct_DSLL    (* 64-bit logical shift left *) [@value 0x38]
    (* 0x39 is unused *)
    | Funct_DSRL    (* 64-bit logical shift right *) [@value 0x3A]
    | Funct_DSRA    (* 64-bit arithmetic shift right *)
    | Funct_DSLL32  (* 64-bit logical shift left plus 32 *)
    | Funct_DSRL32  (* 64-bit logical shift right plus 32 *)
    | Funct_DSRA32  (* 64-bit arithmetic shift right plus 32 *)
    *)
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
    rd    : register;
    shamt : int;
    funct : funct;
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
    | Mips_ALU  | Mips_MFC0 -> Some Optype_R
    | Mips_J    | Mips_JAL  -> Some Optype_J
    | Mips_BEQ  | Mips_BNE   | Mips_BLEZ | Mips_ADDI | Mips_ADDIU
    | Mips_SLTI | Mips_SLTIU | Mips_ANDI | Mips_ORI  | Mips_LUI
    | Mips_LW   | Mips_LBU   | Mips_LHU  | Mips_SB   | Mips_SH
    | Mips_SW  -> Some Optype_I

let decode_r inst =
    let open Stdint.Uint32 in
    let (>>) = shift_right_logical in
    let (land) = logand in
    let fivebits = of_int 0x1F in
    let sixbits = of_int 0x3F in
    let funct = inst land sixbits |> to_int |> funct_of_enum |> CCOpt.get_or ~default:Funct_SLL in
    let shamt = (inst >> 6) land fivebits |> to_int in
    (* The following should never fail *)
    let rd = (inst >> 11) land fivebits |> to_int |> register_of_enum |> CCOpt.get_or ~default:R0 in
    let rt = (inst >> 16) land fivebits |> to_int |> register_of_enum |> CCOpt.get_or ~default:R0 in
    let rs = (inst >> 21) land fivebits |> to_int |> register_of_enum |> CCOpt.get_or ~default:R0 in
    (* But this could. *)
    match inst >> 26 |> to_int |> opcode_of_enum with
    | Some op -> Inst_R { op; rs; rt; rd; shamt; funct }
    | None -> failwith "Illegal instruction"

let decode s =
    assert (Bytes.length s = 4);
    let open Stdint.Uint32 in
    let inst = of_bytes_little_endian s 0 in
    let op = shift_right_logical inst 26 in
    match CCOpt.(op |> to_int |> opcode_of_enum >>= type_of_opcode) with
    | Some optype ->
        begin match optype with
        | Optype_R -> decode_r inst
      (*| Optype_I -> Opcode_i.decode inst*)
      (*| Optype_J -> Opcode_j.decode inst*)
        | _ -> failwith "Opcodes with formats other than R are not yet implemented"
        end
    | None -> failwith "Illegal instruction"
