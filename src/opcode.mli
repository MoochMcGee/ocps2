(** A MIPS register *)
type register =
    | R_zero (* always zero *)
    | R_at | R_v0 | R_v1 | R_a0 | R_a1 | R_a2 | R_a3
    | R_t0 | R_t1 | R_t2 | R_t3 | R_t4 | R_t5 | R_t6 | R_t7
    | R_s0 | R_s1 | R_s2 | R_s3 | R_s4 | R_s5 | R_s6 | R_s7
    | R_t8 | R_t9 | R_k0 | R_k1 | R_gp | R_sp | R_fp | R_ra
    [@@deriving enum, show]

(** A MIPS opcode mnemonic *)
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

(** A MIPS instruction.
 * Decoding is broken down by instruction format.
 *)
type t =
    | Inst_R of inst_r
    | Inst_I of inst_i
    | Inst_J of inst_j
    | NYI
    [@@deriving show]

(** Decode a MIPS instruction *)
val decode : Bytes.t -> t
