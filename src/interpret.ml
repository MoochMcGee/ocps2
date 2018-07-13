open Stdint

let create () =
    let reg   = Array.make 32 Int32.zero in
    let reghi = Array.make 32 Int32.zero in
    let pc    = ref @@ Uint32.of_string "0xbfc00000" in
    let hi    = ref Int32.zero in
    let hi1   = ref Int32.zero in
    let lo    = ref Int32.zero in
    let lo1   = ref Int32.zero in
    let (proc : Processor.t) = { reg; reghi; pc; hi; hi1; lo; lo1 } in
    proc

let execute_r proc inst = function
    | Opcode.Mips_ALU -> Interpret_alu.execute proc inst
    | _ -> failwith "Execution of non-ALU instructions not yet implemented"

let execute proc _mem = function
    | Opcode.Inst_R inst -> execute_r proc inst inst.op
    | _ -> failwith "Execution of non-R formats not yet implemented"

let step (proc : Processor.t) mem =
    let instruction = Memory.read 4 mem (Uint32.to_int !(proc.pc)) in
    let instruction = Opcode.decode instruction in
    Printf.printf "%s\n" (Opcode.show instruction);
    proc.pc := execute proc mem instruction
