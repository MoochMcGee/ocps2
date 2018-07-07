let test_add () =
    (* add $t3, $t1, $t2 *)
    let add = "\x20\x58\x2a\x01" in
    let op = Bytes.of_string add |> Opcode.decode in
    Printf.printf "Insn: %s\n" (Opcode.show op);
    match op with
    | Opcode.Inst_R op ->
        begin match op.op with
        | Opcode.Mips_ALU ->
            begin match op.rd with
            | Opcode.R_t3 ->
                begin match op.rs with
                | Opcode.R_t1 ->
                    begin match op.rt with
                    | Opcode.R_t2 ->
                        Printf.printf "It works!\n"
                    | _ -> failwith "rt is wrong"
                    end
                | _ -> failwith "rs is wrong"
                end
            | _ -> failwith "rd is wrong"
            end
        | _ -> failwith "op is wrong"
        end
    | _ -> failwith "instruction format is wrong"

let test_addi () =
    (* addi $t1, 10($zero) *)
    let addi = "\x0a\x00\x09\x20" in
    let op = Bytes.of_string addi |> Opcode.decode in
    Printf.printf "Insn: %s\n" (Opcode.show op);
    match op with
    | Opcode.Inst_I op ->
        begin match op.op with
        | Opcode.Mips_ADDI ->
            begin match op.rt with
            | Opcode.R_t1 ->
                begin match op.rs with
                | Opcode.R_zero ->
                    begin match op.imm with
                    | 10 ->
                        Printf.printf "It works!\n"
                    | _ -> failwith "imm is wrong"
                    end
                | _ -> failwith "rs is wrong"
                end
            | _ -> failwith "rt is wrong"
            end
        | _ -> failwith "op is wrong"
        end
    | _ -> failwith "instruction format is wrong"

let () =
    test_add ();
    test_addi ()
