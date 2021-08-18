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

module Int64_binary : Binary with type t := int64 = struct
  let slurp bs offset   = ok (offset + 8,get_int64_be bs offset)
  let barf  bs offset i = let () = set_int64_be bs offset i in offset + 8
end

module Int32_binary : Binary with type t := int32 = struct
  let slurp bs offset   = ok (offset + 4,get_int32_be bs offset)
  let barf  bs offset i = let () = set_int32_be bs offset i in offset + 4
end

module UInt16_binary : Binary with type t := int = struct
  let slurp bs offset   = ok (offset + 2,get_uint16_be bs offset)
  let barf  bs offset i = let () = set_uint16_be bs offset i in offset + 2
end

module Bytes20_binary : Binary with type t := bytes = struct
  let slurp bs offset   = ok (offset + 20,sub bs offset @@ offset + 20)
  let barf  bs offset i = let () = blit i 0 bs offset 20 in offset + 20
end

module Event_binary : Binary with type t := event = struct
  let int2Event = function
    | 0 -> ok None
    | 1 -> ok Completed
    | 2 -> ok Started
    | 3 -> ok Stopped
    | x -> error @@ UnknownEvent x

  let event2Int = function
    | None      -> 0
    | Completed -> 1
    | Started   -> 2
    | Stopped   -> 3

  let slurp bs offset =
    let* (offset,eventIdx) = Int32_binary.slurp bs offset in
    let* event             = int2Event @@ Int32.to_int eventIdx in
    ok (offset,event)

  let barf bs offset i = Int32_binary.barf bs offset @@ Int32.of_int @@ event2Int i
end

(* Not ideal.
   I don't want a binary instance per gadt constructor here.
   Instead I have a Functor and a module-type that serves as its argument,
   to create an instance for each constructor on the fly.
 *)
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

module Connect_request_payload_binary : Binary with type t := connect Request.payload = struct
  open Request
  let slurp _ offset   = ok (offset,Connect)
  let barf  _ offset _ = offset
end

module Connect_response_payload_binary : Binary with type t := connect Response.payload = struct
  include Response

  let slurp bs offset =
    let* (offset,connection_id) = Int64_binary.slurp bs offset
    in ok (offset, Connect {connection_id = connection_id})

  let barf bs offset (Connect x) = Int64_binary.barf bs offset x.connection_id
end

module Announce_request_payload_binary : (Binary with type t := announce Request.payload) = struct
  open Request

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
    ok (offset, Announce { info_hash  = InfoHash info_hash;
                           peer_id    = PeerId peer_id;
                           downloaded = downloaded;
                           left       = left;
                           uploaded   = uploaded;
                           event      = event;
                           ip         = IPv4 ip;
                           key        = key;
                           num_want   = num_want;
                           port       = port; })

  let barf bs offset (Announce x) =
    let (InfoHash info_hash) = x.info_hash in
    let (PeerId peer_id)     = x.peer_id in
    let (IPv4 ip)            = x.ip in
    let offset = Bytes20_binary.barf bs offset info_hash in
    let offset = Bytes20_binary.barf bs offset peer_id in
    let offset = Int64_binary.barf bs offset x.downloaded in
    let offset = Int64_binary.barf bs offset x.left in
    let offset = Int64_binary.barf bs offset x.uploaded in
    let offset = Event_binary.barf bs offset x.event in
    let offset = Int32_binary.barf bs offset ip in
    let offset = Int32_binary.barf bs offset x.key in
    let offset = Int32_binary.barf bs offset x.num_want in
    UInt16_binary.barf bs offset x.port
end

module Announce_response_payload_binary : Binary with type t := announce Response.payload = struct
  open Response

  let rec slurpPeers peers bs = function
    | offset when offset >= length bs -> ok (offset,peers)
    | offset ->
       let* (offset,ip)   = Int32_binary.slurp bs offset in
       let* (offset,port) = UInt16_binary.slurp bs offset in
       let peers          = {ip = IPv4 ip; port = port} :: peers in
       slurpPeers peers bs offset

  let slurp bs offset =
    let* (offset,interval) = Int32_binary.slurp bs offset in
    let* (offset,leechers) = Int32_binary.slurp bs offset in
    let* (offset,seeders)  = Int32_binary.slurp bs offset in
    let* (offset,peers)    = slurpPeers [] bs offset in
    ok (offset,Announce { interval = interval;
                          leechers = leechers;
                          seeders  = seeders;
                          peers    = peers })

  let rec barf_peers bs offset = function
    | (peer :: peers) ->
       let (IPv4 ip) = peer.ip in
       let offset = Int32_binary.barf bs offset ip in
       let offset = UInt16_binary.barf bs offset peer.port in
       barf_peers bs offset peers
    | _ -> offset

  let barf bs offset (Announce x)=
    let offset = Int32_binary.barf bs offset x.interval in
    let offset = Int32_binary.barf bs offset x.leechers in
    let offset = Int32_binary.barf bs offset x.seeders in
    barf_peers bs offset x.peers
end

module Scrape_response_payload_binary : Binary with type t := scrape Response.payload = struct
  open Response

  let slurp bs offset =
    let* (offset,complete)   = Int32_binary.slurp bs offset in
    let* (offset,downloaded) = Int32_binary.slurp bs offset in
    let* (offset,incomplete) = Int32_binary.slurp bs offset in
    ok (offset,Scrape { complete   = complete;
                        downloaded = downloaded;
                        incomplete = incomplete; })

  let barf bs offset (Scrape x) =
    let offset = Int32_binary.barf bs offset x.complete in
    let offset = Int32_binary.barf bs offset x.downloaded in
    Int32_binary.barf bs offset x.incomplete
end

module Error_msg_response_payload_binary : Binary with type t := error Response.payload = struct
  open Response

  let slurp bs offset =
    let len = length bs in
    ok (len,Error { error = sub_string bs offset @@ len - offset })

  let barf bs offset (Error x) =
    let len = String.length x.error in
    let ()  = blit_string x.error 0 bs offset len in
    len
end

module Binary_header (M : Message_type) : Binary with type t := M.t header = struct
  let slurp bs offset =
    let* (offset,action)         = M.Action.slurp bs offset in
    let* (offset,transaction_id) = Int32_binary.slurp bs offset in
    ok (offset,{ action = action; transaction_id = transaction_id; })

  let barf bs offset x =
    let offset = M.Action.barf bs offset x.action in
    Int32_binary.barf bs offset x.transaction_id
end

module Binary_request (M : Message_type) : Binary with type t := M.t request = struct
  module H = Binary_header(M)

  let slurp bs offset =
    let* (offset,connection_id) = Int64_binary.slurp bs offset in
    let* (offset,header)        = H.slurp bs offset in
    let* (offset,payload)       = M.Request.slurp bs offset in
    Ok (offset,Request (connection_id,header,payload))

  let barf bs offset (Request (connection_id,header,payload)) =
    let offset = Int64_binary.barf bs offset connection_id in
    let offset = H.barf bs offset header in
    M.Request.barf bs offset payload
end

module Binary_response (M : Message_type) : Binary with type t := M.t response = struct
  module H = Binary_header(M)

  let slurp bs offset =
    let* (offset,header)  = H.slurp bs offset in
    let* (offset,payload) = M.Response.slurp bs offset in
    Ok (offset,(Response (header,payload)))

  let barf bs offset (Response (header,payload)) =
    let offset = H.barf bs offset header in
    M.Response.barf bs offset payload
end
