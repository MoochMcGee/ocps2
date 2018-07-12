open Stdint

type bytearray = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
    main   : bytearray; (* 256MiB *)
    ee_reg : bytearray; (* 32MiB *)
    gs_reg : bytearray; (* 32MiB *)
    rom    : bytearray; (* 4MiB *)
}

let create romfile =
    let open Bigarray in
    (* I chunk the memory map into separate sections to reduce usage *)
    (* Main memory; 256MiB *)
    let main = Array1.create Int8_unsigned c_layout 0x1000_0000 in
    Array1.fill main 0;
    (* Emotion Engine registers; 32MiB *)
    let ee_reg = Array1.create Int8_unsigned c_layout 0x200_0000 in
    Array1.fill ee_reg 0;
    (* Graphics Synthesizer registers; 32MiB *)
    let gs_reg = Array1.create Int8_unsigned c_layout 0x200_0000 in
    Array1.fill gs_reg 0;
    (* Boot ROM; 4MiB *)
    let rom = Array1.create Int8_unsigned c_layout 0x40_000 in
    let ic = open_in romfile in
    let idx = ref 0 in
    let rom_size = 0x40_000 in
    let size = 4096 in
    let rec loop () =
        let buf = Bytes.make size '\x00' in
        let _count = input ic buf 0 size in
        Bytes.iteri (fun i c -> int_of_char c |> Array1.set rom (!idx + i)) buf;
        idx := !idx + size;
        if !idx < rom_size then
            loop ()
        else
            ()
    in
    begin try
        loop ();
    with End_of_file ->
        close_in ic;
    end;
    { main; ee_reg; gs_reg; rom }

let start_of_main_memory = 0x0000_0000
let end_of_main_memory = 0x1000_0000 - 1
let start_of_ee_registers = 0x1000_0000
let end_of_ee_registers = 0x1200_0000 - 1
let start_of_gs_registers = 0x1200_0000
let end_of_gs_registers = 0x1400_0000 - 1
let start_of_boot_rom = 0x1fc0_0000
let end_of_boot_rom = 0x2000_0000 - 1

let read' size mem location =
    let (lor) = Uint32.logor in
    let (lsl) = Uint32.shift_left in
    match size with
    | 1 ->
        Uint32.of_int mem.{location}
    | 2 ->
        Uint32.of_int mem.{location} lor
        ((Uint32.of_int mem.{location + 1}) lsl 8)
    | 4 ->
        Uint32.of_int mem.{location} lor
        ((Uint32.of_int mem.{location + 1}) lsl 8) lor
        ((Uint32.of_int mem.{location + 2}) lsl 16) lor
        ((Uint32.of_int mem.{location + 3}) lsl 24)
    | _ -> invalid_arg "Invalid size read; probably a bug"

let read size mem location =
    let location = location land 0x1fff_ffff in
    if location >= start_of_main_memory && location <= end_of_main_memory then
        read' size mem.main location
    else if location >= start_of_ee_registers && location <= end_of_ee_registers then
        let location = location - start_of_ee_registers in
        read' size mem.ee_reg location
    else if location >= start_of_gs_registers && location <= end_of_gs_registers then
        let location = location - start_of_gs_registers in
        read' size mem.gs_reg location
    else if location >= start_of_boot_rom && location <= end_of_boot_rom then
        let location = location - start_of_boot_rom in
        read' size mem.rom location
    else 
        let error = Printf.sprintf "Bus error while reading %d bytes from 0x%x" size location in
        failwith error

let write' size mem location value =
    let (lsl) = Uint32.shift_left in
    let (land) = Uint32.logand in
    let byte = Uint32.of_int 0xFF in
    match size with
    | 1 ->
        let value = value land byte |> Uint32.to_int in
        mem.{location} <- value
    | 2 ->
        let value0 = value land byte |> Uint32.to_int in
        let value1 = (value lsl 8) land byte |> Uint32.to_int in
        mem.{location} <- value0;
        mem.{location + 1} <- value1
    | 4 ->
        let value0 = value land byte |> Uint32.to_int in
        let value1 = (value lsl 8) land byte |> Uint32.to_int in
        let value2 = (value lsl 16) land byte |> Uint32.to_int in
        let value3 = (value lsl 24) land byte |> Uint32.to_int in
        mem.{location} <- value0;
        mem.{location + 1} <- value1;
        mem.{location + 2} <- value2;
        mem.{location + 3} <- value3
    | _ -> invalid_arg "Invalid size write; probably a bug"

let write size mem location value =
    if location >= start_of_main_memory && location <= end_of_main_memory then
        write' size mem.main location value
    else if location >= start_of_ee_registers && location <= end_of_ee_registers then
        let location = location - start_of_ee_registers in
        write' size mem.ee_reg location value
    else if location >= start_of_gs_registers && location <= end_of_gs_registers then
        let location = location - start_of_gs_registers in
        write' size mem.gs_reg location value
    else if location >= start_of_boot_rom && location <= end_of_boot_rom then
        let location = location - start_of_boot_rom in
        write' size mem.rom location value
    else 
        let error = Printf.sprintf "Bus error while writing %d bytes from 0x%x" size location in
        failwith error

