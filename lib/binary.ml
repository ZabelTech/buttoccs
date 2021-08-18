open Bytes
open Result
open Message
let undefined = assert false

type parse_error = NotEnoughData
                 | UnexpectedAction
                 | UnknownEvent of int

let ensure_size s b cont = if Bytes.length b < s then Result.Error NotEnoughData else cont

let (let*) o f =
  match o with
  | Ok r    -> f r
  | Error e -> Result.Error e

module type Binary = sig
  type t
  val slurp : bytes -> int -> (int * t,parse_error) result
  val barf  : bytes -> int -> t -> int
end

module type Message_type = sig
  type t
  module Action   : Binary with type t := t action
  module Request  : Binary with type t := t Request.payload
  module Response : Binary with type t := t Response.payload
end

module Int64_binary : Binary with type t = int64 = struct
  type t = int64
  let slurp bs offset   = ok (offset + 8,get_int64_be bs offset)
  let barf  bs offset i = let _ = set_int64_be bs offset i in offset + 8
end

module Int32_binary : Binary with type t = int32 = struct
  type t = int32
  let slurp bs offset   = ok (offset + 4,get_int32_be bs offset)
  let barf  bs offset i = let _ = set_int32_be bs offset i in offset + 4
end

module UInt16_binary : Binary with type t = int = struct
  type t = int
  let slurp bs offset   = ok (offset + 2,get_uint16_be bs offset)
  let barf  bs offset i = let _ = set_uint16_be bs offset i in offset + 2
end

module Bytes20_binary : Binary with type t = bytes = struct
  type t = bytes
  let slurp bs offset   = ok (offset + 20,sub bs offset @@ offset + 20)
  let barf  bs offset i = let _ = blit i 0 bs offset 20 in offset + 20
end

module Event_binary : Binary with type t = event = struct
  type t = event

  let int2Event = function
    | 0 -> ok None
    | 1 -> ok Completed
    | 2 -> ok Started
    | 3 -> ok Stopped
    | x -> error @@ UnknownEvent x

  let slurp bs offset =
    let* (offset,eventIdx) = Int32_binary.slurp bs offset in
    let* event             = int2Event @@ Int32.to_int eventIdx in
    ok (offset,event)

  let barf = undefined
end

module type SomeAction = sig
  type t
  val constructor : t action
  val idx : int
end

module Action_binary (A : SomeAction) : Binary = struct
  type t = A.t action
  let slurp bs offset =
    let* (offset,res) = Int32_binary.slurp bs offset
    in if Int32.to_int res = A.idx
       then ok (offset,A.constructor)
       else error UnexpectedAction
  let barf bs offset _ = Int32_binary.barf bs offset @@ Int32.of_int @@ A.idx
end

module Connect_request_payload_binary : Binary with type t = connect Request.payload = struct
  type t               = connect Request.payload
  let slurp _ offset   = ok (offset,Request.Connect)
  let barf  _ offset _ = offset
end

module Connect_response_payload_binary : Binary with type t = connect Response.payload = struct
    type t = connect Response.payload

    let slurp bs offset =
      let* (offset,connection_id) = Int64_binary.slurp bs offset
      in ok (offset, Response.Connect {connection_id = connection_id})

    let barf bs offset = function
      | Response.Connect x -> Int64_binary.barf bs offset x.connection_id
      | _ -> undefined
end

module Announce_request_payload_binary : Binary with type t = announce Request.payload = struct
    type t = announce Request.payload

    let slurp bs offset =
      let* (offset,info_hash)  = Bytes20_binary.slurp bs offset in
      let* (offset,peer_id)    = Bytes20_binary.slurp bs offset in
      let* (offset,downloaded) = Int64_binary.slurp bs offset in
      let* (offset,left)       = Int64_binary.slurp bs offset in
      let* (offset,uploaded)   = Int64_binary.slurp bs offset in
      let* (offset,event)      = Event_binary.slurp bs offset in
      let* (offset,ip)         = Int32_binary.slurp bs offset in
      let* (offset,key)        = Int32_binary.slurp bs offset in
      let* (offset,num_want)   = Int32_binary.slurp bs offset in
      let* (offset,port)       = UInt16_binary.slurp bs offset in

      ok (offset,Request.Announce { info_hash  = InfoHash info_hash;
                                    peer_id    = PeerId peer_id;
                                    downloaded = downloaded;
                                    left       = left;
                                    uploaded   = uploaded;
                                    event      = event;
                                    ip         = IPv4 ip;
                                    key        = key;
                                    num_want   = num_want;
                                    port       = port; })

    let barf = undefined
end

module Announce_response_payload_binary : Binary with type t = announce Response.payload = struct
    type t = announce Response.payload

    let rec slurpPeers peers bs = function
      | offset when offset >= length bs -> ok (offset,peers)
      | offset ->
         let* (offset,ip)   = Int32_binary.slurp bs offset in
         let* (offset,port) = UInt16_binary.slurp bs offset in
         let peers          = [{ip = IPv4 ip; port = port}] in
         slurpPeers peers bs offset

    let slurp bs offset =
      let* (offset,interval) = Int32_binary.slurp bs offset in
      let* (offset,leechers) = Int32_binary.slurp bs offset in
      let* (offset,seeders)  = Int32_binary.slurp bs offset in
      let* (offset,peers)    = slurpPeers [] bs offset in
      ok (offset,Response.Announce { interval = interval;
                                     leechers = leechers;
                                     seeders  = seeders;
                                     peers    = peers })

    let barf = undefined
end

module Scrape_response_payload_binary : Binary with type t = scrape Response.payload = struct
    type t = scrape Response.payload

    let slurp bs offset =
      let* (offset,complete)   = Int32_binary.slurp bs offset in
      let* (offset,downloaded) = Int32_binary.slurp bs offset in
      let* (offset,incomplete) = Int32_binary.slurp bs offset in
      ok (offset,Response.Scrape { complete   = complete;
                                   downloaded = downloaded;
                                   incomplete = incomplete; })

    let barf = undefined
end

module Error_msg_response_payload_binary : Binary with type t = error_msg Response.payload = struct
    type t = error_msg Response.payload

    let slurp bs offset =
      let len = length bs in
      ok (len,Response.ErrorMsg { error = sub_string bs offset @@ len - offset })

    let barf = undefined
end

module Binary_header (M : Message_type) : Binary with type t = M.t header = struct
  type t = M.t header
  let slurp bs offset =
    let* (offset,action)         = M.Action.slurp bs offset in
    let* (offset,transaction_id) = Int32_binary.slurp bs offset in
    ok (offset,{ action = action; transaction_id = transaction_id; })

  let barf  = undefined
end

module Binary_request (M : Message_type) : Binary with type t = M.t request = struct
  type t   = M.t request
  module H = Binary_header(M)

  let slurp bs offset =
      let* (offset,connection_id) = Int64_binary.slurp bs offset in
      let* (offset,header)        = H.slurp bs offset in
      let* (offset,payload)       = M.Request.slurp bs offset in
      Ok (offset,Request (connection_id,header,payload))

  let barf  _ = undefined
end

module Binary_response (M : Message_type) : Binary with type t = M.t response = struct
  type t   = M.t response
  module H = Binary_header(M)
  let slurp bs offset =
      let* (offset,header)  = H.slurp bs offset in
      let* (offset,payload) = M.Response.slurp bs offset in
      Ok (offset,(Response (header,payload)))

  let barf  _ = undefined
end
