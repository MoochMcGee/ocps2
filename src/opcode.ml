type register =
    | R_zero (* always zero *)
    | R_at | R_v0 | R_v1 | R_a0 | R_a1 | R_a2 | R_a3
    | R_t0 | R_t1 | R_t2 | R_t3 | R_t4 | R_t5 | R_t6 | R_t7
    | R_s0 | R_s1 | R_s2 | R_s3 | R_s4 | R_s5 | R_s6 | R_s7
    | R_t8 | R_t9 | R_k0 | R_k1 | R_gp | R_sp | R_fp | R_ra
    [@@deriving enum, show]

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
    [@@deriving enum, show]

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
    [@@deriving enum, show]

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
} [@@deriving show]

(* integer: register and immediate to register
 * OP rt, IMM(rs)
 * OP rs, rt, IMM (beq/bne)
 * encoded as
 * 31   25             20              15          0
 * | op | first source | second source | immediate |
 *)
type inst_i = {
    op    : opcode;
    rs    : register;
    rt    : register;
    imm   : int;
} [@@deriving show]

(* jump
 * OP label
 * encoded as
 * 31   25            0
 * | op | destination |
 *)
type inst_j = {
    op    : opcode;
    dest  : int;
} [@@deriving show]

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
    | Inst_I of inst_i
    | Inst_J of inst_j
    | NYI
    [@@deriving show]

let type_of_opcode = function
    | Mips_ALU  | Mips_MFC0 -> Some Optype_R
    | Mips_J    | Mips_JAL  -> Some Optype_J
    | Mips_BEQ  | Mips_BNE   | Mips_BLEZ | Mips_ADDI | Mips_ADDIU
    | Mips_SLTI | Mips_SLTIU | Mips_ANDI | Mips_ORI  | Mips_LUI
    | Mips_LW   | Mips_LBU   | Mips_LHU  | Mips_SB   | Mips_SH
    | Mips_SW  -> Some Optype_I

let get_op inst =
    let open Stdint.Uint32 in
    let (>>) = shift_right_logical in
    inst >> 26 |> to_int |> opcode_of_enum 

let extract_register ~shift inst =
    let open Stdint.Uint32 in
    let (>>) = shift_right_logical in
    let (land) = logand in
    let fivebits = of_int 0x1F in
    (inst >> shift) land fivebits |> to_int |> register_of_enum |> CCOpt.get_or ~default:R_zero

let get_rs =
    extract_register ~shift:21

let get_rt =
    extract_register ~shift:16

let decode_r inst =
    let open Stdint.Uint32 in
    let (>>) = shift_right_logical in
    let (land) = logand in
    let fivebits = of_int 0x1F in
    let sixbits = of_int 0x3F in
    (* Should this throw illegal instruction? *)
    let funct = inst land sixbits |> to_int |> funct_of_enum |> CCOpt.get_or ~default:Funct_SLL in
    let shamt = (inst >> 6) land fivebits |> to_int in
    (* The following should never fail *)
    let rd = extract_register ~shift:11 inst in
    let rt = get_rt inst in 
    let rs = get_rs inst in
    (* But this could. *)
    match get_op inst with
    | Some Mips_ALU -> Inst_R { op = Mips_ALU; rs; rt; rd; shamt; funct }
    (* This could be a bug if we reach this. *)
    | None -> failwith "Illegal instruction: unrecognised opcode in R-type instruction"
    | Some op -> failwith "Illegal instruction: R-type opcodes only use the ALU op"

let decode_i inst =
    let open Stdint.Uint32 in
    let (land) = logand in
    let twobytes = of_int 0xFFFF in
    (* The following should never fail *)
    let imm = inst land twobytes |> to_int in
    let rt = get_rt inst in
    let rs = get_rs inst in
    (* But this could. *)
    match get_op inst with
    | Some op ->
        begin match op with
        | Mips_BEQ  | Mips_BNE   | Mips_BLEZ | Mips_ADDI | Mips_ADDIU
        | Mips_SLTI | Mips_SLTIU | Mips_ANDI | Mips_ORI  | Mips_LUI
        | Mips_LW   | Mips_LBU   | Mips_LHU  | Mips_SB   | Mips_SH
        | Mips_SW ->
            Inst_I { op; rs; rt; imm }
        (* This could be a bug if we reach this. *)
        | _ ->
            failwith "Illegal instruction: invalid opcode in I-type instruction"
        end
    | None ->
        failwith "Illegal instruction: unrecognised opcode in I-type instruction"

let decode_j inst =
    let open Stdint.Uint32 in
    let (land) = logand in
    let twentysixbits = of_int 0x02FFFFFF in
    (* The following should never fail *)
    let dest = inst land twentysixbits |> to_int in
    (* But this could. *)
    match get_op inst with
    | Some op ->
        begin match op with
        | Mips_J | Mips_JAL -> 
            Inst_J { op; dest }
        (* This could be a bug if we reach this. *)
        | _ ->
            failwith "Illegal instruction: invalid opcode in J-type instruction"
        end
    | None ->
        failwith "Illegal instruction: unrecognised opcode in J-type instruction"

let decode s =
    assert (Bytes.length s = 4);
    let open Stdint.Uint32 in
    let inst = of_bytes_little_endian s 0 in
    match CCOpt.(get_op inst >>= type_of_opcode) with
    | Some optype ->
        begin match optype with
        | Optype_R -> decode_r inst
        | Optype_I -> decode_i inst
        | Optype_J -> decode_j inst
        (*| _ -> failwith "Opcodes with formats other than R, I and J are not yet implemented"*)
        end
    | None -> failwith "Illegal instruction"
