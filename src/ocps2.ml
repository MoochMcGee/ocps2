let () =
    (* add $t3, $t1, $t2 *)
    let add = "\x20\x58\x2a\x01" in
    let op = Bytes.of_string add |> Opcode.decode in
    match op with
    | Opcode.Inst_R op ->
        begin match op.op with
        | Opcode.Mips_ALU ->
            begin match op.rd with
            | Opcode.R11 ->
                begin match op.rs with
                | Opcode.R9 ->
                    begin match op.rt with
                    | Opcode.R10 ->
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
