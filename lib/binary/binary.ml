open Util
open Bytes
open Monad
open Result

type binary_error = ..

type binary_error += Not_enough_bytes

type 'a monad_binary = ('a,binary_error) result rs_monad

let ensure_size n = get_chunk n >>= function
  | Some (bs,offset) -> succeed (bs,offset)
  | None             -> fail Not_enough_bytes

module type Binary = sig
  type t
  val slurp   : t monad_binary
  val barf    : t -> unit monad_binary
  val size_of : t option -> int
end

module type Binary_spec = sig
  type t
  val size_of     : t option -> int
  val slurp_chunk : bytes -> int -> (t,binary_error) result
  val barf_chunk  : bytes -> int -> t -> unit
end

module Mk_binary(M : Binary_spec) : Binary with type t = M.t = struct
  type t      = M.t
  let size_of = M.size_of

  let slurp =
    let** (bs,offset) = ensure_size @@ M.size_of None in
    return @@ M.slurp_chunk bs offset

  let barf x =
    let** (bs,offset) = ensure_size @@ M.size_of (Some x) in
    succeed @@ M.barf_chunk bs offset x
end

module Int64_binary = Mk_binary(struct
  type t          = int64
  let size_of _   = 8
  let slurp_chunk = safe get_int64_be
  let barf_chunk  = set_int64_be
end)

module Int32_binary = Mk_binary(struct
  type t          = int32
  let size_of _   = 4
  let slurp_chunk = safe get_int32_be
  let barf_chunk  = set_int32_be
end)

module UInt16_binary = Mk_binary(struct
  type t          = Unsigned.UInt16.t
  let size_of _   = 2
  let slurp_chunk bs       = ok << Unsigned.UInt16.of_int << get_uint16_be bs
  let barf_chunk bs offset = set_uint16_be bs offset << Unsigned.UInt16.to_int
end)

module Bytes20_binary = Mk_binary(struct
  type t          = bytes
  let size_of _   = 20
  let slurp_chunk bs offset  = ok @@ sub bs offset @@ size_of None
  let barf_chunk bs offset x = blit x 0 bs offset @@ size_of (Some x)
end)

module String_binary : Binary with type t := string = struct
  let size_of = Option.fold ~none:0 ~some:String.length

  let slurp =
    let** (bytes,offset) = lift @@ get_rest in
    succeed @@ sub_string bytes offset @@ Bytes.length bytes - offset

  let barf x =
    let len           = String.length x in
    let** (bs,offset) = ensure_size @@ size_of (Some x) in
    succeed @@ blit_string x 0 bs offset len
end
