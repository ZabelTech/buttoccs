open Result
(* A poor man's ad-hoc Monad-stack implementation.
   Run with this for now. It's definitly better than the prior status quo i.e. explicit parameter passing.
   Maybe reconsider this judgement in the future,
   as it might be more idiomatic to wrap bytes+offset in an object.
 *)

type offset       = int
type binary_error = NotEnoughData
type 'a rs_monad  = bytes -> offset -> (offset * 'a)

let (>>=) m_value fn bytes offset =
  let (new_offset,return_value) = m_value bytes offset in
  fn return_value bytes new_offset

let return x _ s = (s,x)

let is_empty bytes offset =
  if Bytes.length bytes <= offset then (offset,true) else (offset,false)

let get_chunk n bytes offset =
  if Bytes.length bytes - offset < n then (offset,None) else (offset + n, Some (bytes,offset))

let get_rest bytes offset = (Bytes.length bytes,(bytes,offset))

let succeed x = return @@ ok x
let fail e    = return @@ error e
let lift x    = x >>= succeed
let safe fn bs offset = ok @@ fn bs offset

let (let*) = (>>=)

(* Binding to add generic error handling ontop of monad stack *)
let (let**) m_value f =
  m_value >>= function
  | Ok r    -> f r
  | Error e -> fail e
