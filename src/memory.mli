(** An abbreviated Bigarray *)
type bytearray = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** The memory of a PlayStation 2. *)
type t = {
    main   : bytearray; (* 256MiB *)
    ee_reg : bytearray; (* 32MiB *)
    gs_reg : bytearray; (* 32MiB *)
    rom    : bytearray; (* 4MiB *)
}

(** Initialise the main memory of a PlayStation 2. *)
val create : string -> t

(** [read size mem location] reads [size] bytes from [mem] at [location] after
 * memory address translation. Raises Invalid_argument if [size] is not
 * 1, 2 or 4, and Failure if [location] is not a valid memory region. *)
val read : int -> t -> int -> Stdint.Uint32.t

(** [write size mem location value] writes [size] bytes of [value] to [mem] at
 * [location] after memory address translation. Raises Invalid_argument if
 * [size] is not 1, 2 or 4, and Failure if [location] is not a valid memory
 * region. *)
val write : int -> t -> int -> Stdint.Uint32.t -> unit

