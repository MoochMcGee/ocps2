open Stdint

(* The interpreter returns the number of bytes to advance by.
 * Each MIPS instruction is 4 bytes, so to advance to the next instruction
 * you have to add 4 to the program counter.
 *)
let next_instruction proc =
    proc.pc + 4

(* Reading from $zero always returns zero, so handle that specially. *)
let reg_read proc reg =
    if reg = 0 then
        0
    else
        proc.reg.(reg)

(* Writing to $zero always gets discarded, so handle that specially. *)
let reg_write proc reg value =
    if reg <> 0 then
        proc.reg.(reg) <- value

(* Helper function for shift instructions *)
let shift proc ~src ~dst ~amount ~f =
    let data = f (reg_read proc src) amount in
    reg_write proc dst data;
    next_instruction proc

(* Shift left logical *)
let sll proc ~rd ~rt ~shamt =
    shift proc ~src:rt ~dst:rd ~amount:shamt ~f:Int32.shift_left

(* Shift right logical *)
let srl proc ~rd ~rt ~shamt =
    shift proc ~src:rt ~dst:rd ~amount:shamt ~f:Int32.shift_right

(* Shift right arithmetic *)
let sra proc ~rd ~rt ~shamt =
    shift proc ~src:rt ~dst:rd ~amount:shamt ~f:Int32.shift_right_arithmetic

(* Shift left logical by variable *)
let sllv proc ~rd ~rs ~rt =
    shift proc ~src:rs ~dst:rd ~amount:(reg_read proc rt) ~f:Int32.shift_left

(* Shift right logical by variable *)
let srlv proc ~rd ~rs ~rt =
    shift proc ~src:rs ~dst:rd ~amount:(reg_read proc rt) ~f:Int32.shift_right

(* Shift right arithmetic by variable *)
let srav proc ~rd ~rs ~rt =
    shift proc ~src:rs ~dst:rd ~amount:(reg_read proc rt) ~f:Int32.shift_right_arithmetic

(* Jump to register *)
let jr proc ~rs =
    Uint32.of_int32 (reg_read proc rs)

(* Jump and link register *)
let jalr proc ~rs =
    (* Store next instruction in $ra *)
    reg_write proc (Opcode.register_to_enum Opcode.R_ra) (next_instruction proc);
    jr proc ~rs

(* Move from $HI *)
let mfhi proc ~rd =
    reg_write proc rd proc.hi;
    next_instruction proc

(* Move to $HI *)
let mthi proc ~rs =
    proc.hi := reg_read proc rs;
    next_instruction proc

(* Move from $LO *)
let mflo proc ~rd =
    reg_write proc rd proc.lo;
    next_instruction proc

(* Move to $LO *)
let mtlo proc ~rs =
    proc.lo := reg_read proc rs;
    next_instruction proc

(* Multiply *)
let mult proc ~rs ~rt =
    let ( * ) = Int32.mul in
    proc.lo := (reg_read proc rs) * (reg_read proc rt);
    next_instruction proc

(* Multiply unsigned *)
let multu proc ~rs ~rt =
    let ( * ) = Uint32.mul in
    proc.lo := Int32.of_uint32 ((Uint32.of_int32 @@ reg_read proc rs) * (Uint32.of_int32 @@ reg_read proc rt));
    next_instruction proc

(* Divide *)
let div proc ~rs ~rt =
    let (/) = Int32.div in
    let (mod) = Int32.rem in
    proc.lo := (reg_read proc rs) / (reg_read proc rt);
    proc.hi := (reg_read proc rs) mod (reg_read proc rs);
    next_instruction proc

(* Divide unsigned *)
let div proc ~rs ~rt =
    let (/) = Uint32.div in
    let (mod) = Uint32.rem in
    proc.lo := Int32.of_uint32 ((Uint32.of_int32 @@ reg_read proc rs) / (Uint32.of_int32 @@ reg_read proc rt));
    proc.hi := Int32.of_uint32 ((Uint32.of_int32 @@ reg_read proc rs) mod (Uint32.of_int32 @@ reg_read proc rs));
    next_instruction proc

(* Add *)
let add proc ~rs ~rt ~rd =
    let (+) = Int32.add in
    let rs = Uint32.to_int32 @@ reg_read proc rs in
    let rt = Uint32.to_int32 @@ reg_read proc rt in
    let data = rs + rt in
    if (rs > 0 && rt > 0 && data < 0)
    || (rs < 0 && rt < 0 && data > 0) then
        raise Exception.Integer_overflow;
    let data = Uint32.of_int32 data in
    reg_write proc rd data;
    next_instruction proc

(* Add Unsigned *)
let addu proc ~rs ~rt ~rd =
    let (+) = Uint32.add in
    let data = (reg_read proc rs) + (reg_read proc rt) in
    reg_write proc rd data;
    next_instruction proc

(* Subtract *)
let sub proc ~rs ~rt ~rd =
    let (-) = Int32.sub in
    let data = (Uint32.to_int32 @@ reg_read proc rs) - (Uint32.to_int32 @@ reg_read proc rs) in
    reg_write proc rd data;
    next_instruction proc

let execute proc mem inst =
    let rs = Opcode.register_to_enum inst.rs in
    let rt = Opcode.register_to_enum inst.rt in
    let rd = Opcode.register_to_enum inst.rd in
    let shamt = inst.shamt in
    match inst.funct with
    | Funct_SLL     -> sll   proc ~rt ~rd ~shamt
    | Funct_SRL     -> srl   proc ~rt ~rd ~shamt
    | Funct_SRA     -> sra   proc ~rt ~rd ~shamt
    | Funct_SLLV    -> sllv  proc ~rt ~rd ~shamt
    | Funct_SRLV    -> srlv  proc ~rt ~rd ~shamt
    | Funct_SRAV    -> srav  proc ~rt ~rd ~shamt
    | Funct_JR      -> jr    proc ~rs
    | Funct_JALR    -> jalr  proc ~rs
    | Funct_SYSCALL -> raise Exception.System_call
    | Funct_BREAK   -> failwith "break is not implemented"
    | Funct_MFHI    -> mfhi  proc ~rd
    | Funct_MTHI    -> mthi  proc ~rs
    | Funct_MFLO    -> mflo  proc ~rd
    | Funct_MTLO    -> mtlo  proc ~rs
    | Funct_MULT    -> mult  proc ~rs ~rt
    | Funct_MULTU   -> multu proc ~rs ~rt
    | Funct_DIV     -> div   proc ~rs ~rt
    | Funct_DIVU    -> divu  proc ~rs ~rt
    | Funct_ADD     -> add   proc ~rs ~rt ~rd
    | Funct_ADDU    -> addu  proc ~rs ~rt ~rd
    | Funct_SUB     -> sub   proc ~rs ~rt ~rd
