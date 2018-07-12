open Stdint

type t = {
    reg : Int32.t array;
    pc  : Uint32.t ref;
    hi  : Int32.t ref;
    lo  : Int32.t ref;
}

let create () =
    let reg = Array.make 32 Uint32.zero in
    let pc = ref @@ Uint32.of_string "0xbfc00000" in
    { reg; pc }

let execute_r proc mem inst = function
    | Opcode.Mips_ALU -> Interpret_ALU.execute proc mem inst
    | _ -> failwith "Execution of non-ALU instructions not yet implemented"

let execute proc mem = function
    | Opcode.Inst_R inst -> execute_r proc mem inst inst.op
    | _ -> failwith "Execution of non-R formats not yet implemented"

let step proc mem =
    let instruction = Memory.read 4 mem proc.pc in
    let instruction = Opcode.decode instruction in
    proc.pc := execute proc mem instruction
