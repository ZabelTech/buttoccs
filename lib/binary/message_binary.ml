open Util
open Message
open Monad
open Result
open Binary

type binary_error += Unknown_event of int
type binary_error += Unexpected_action of int

module Event_binary : Binary with type t := event = struct
  let size_of _ = 4

  let int2Event = function
    | 0 -> ok None
    | 1 -> ok Completed
    | 2 -> ok Started
    | 3 -> ok Stopped
    | x -> error @@ Unknown_event x

  let event2Int = function
    | None      -> 0
    | Completed -> 1
    | Started   -> 2
    | Stopped   -> 3

  let slurp =
    let** eventIdx = Int32_binary.slurp in
    return @@ int2Event @@ Stdlib.Int32.to_int eventIdx

  let barf = Int32_binary.barf << Stdlib.Int32.of_int << event2Int
end

module Mk_action_binary(A : Message_type) : Binary with type t := A.a action = struct
  let size_of _ = 4

  let to_int : type a. a action -> int = function
    | Connect  -> 0
    | Announce -> 1
    | Scrape   -> 2
    | Error    -> 3

  let of_int = function
    | i when i == to_int (get_action_sing A.sing) -> ok (get_action_sing A.sing)
    | i -> error @@ Unexpected_action i

  let slurp =
    let** action_idx = Int32_binary.slurp in
    return @@ of_int @@ Stdlib.Int32.to_int action_idx

  let barf x =
    let idx = Stdlib.Int32.of_int @@ to_int x in
    Int32_binary.barf idx
end

module Mk_header_binary (A : Message_type) : Binary with type t := A.a header = struct
  module Action_binary = Mk_action_binary(A)

  let size_of _ = 8

  let slurp =
    let** action         = Action_binary.slurp in
    let** transaction_id = Int32_binary.slurp in
    succeed { action; transaction_id; }

  let barf x =
    let** () = Action_binary.barf x.action in
    Int32_binary.barf x.transaction_id
end

module Connect_request_payload_binary : Binary with type t = connect Request.payload = struct
  open Request
  type t = connect Request.payload
  let size_of _ = 0
  let slurp     = succeed Connect
  let barf _    = succeed ()
end

module Announce_request_payload_binary : Binary with type t = announce Request.payload = struct
  open Request
  type t = announce Request.payload
  let size_of _ = 82

  let slurp =
    let** info_hash  = Bytes20_binary.slurp in
    let** peer_id    = Bytes20_binary.slurp in
    let** downloaded = Int64_binary.slurp in
    let** left       = Int64_binary.slurp in
    let** uploaded   = Int64_binary.slurp in
    let** event      = Event_binary.slurp in
    let** ip         = Int32_binary.slurp in
    let** key        = Int32_binary.slurp in
    let** num_want   = Int32_binary.slurp in
    let** port       = UInt16_binary.slurp in
    succeed @@ Announce { info_hash  = InfoHash info_hash;
                          peer_id    = PeerId peer_id;
                          ip         = IPv4 ip;
                          downloaded; left; uploaded;
                          event; key; num_want; port; }

  let barf (Announce x) =
    let (InfoHash info_hash) = x.info_hash in
    let (PeerId peer_id)     = x.peer_id in
    let (IPv4 ip)            = x.ip in
    let** () = Bytes20_binary.barf info_hash in
    let** () = Bytes20_binary.barf peer_id in
    let** () = Int64_binary.barf x.downloaded in
    let** () = Int64_binary.barf x.left in
    let** () = Int64_binary.barf x.uploaded in
    let** () = Event_binary.barf x.event in
    let** () = Int32_binary.barf ip in
    let** () = Int32_binary.barf x.key in
    let** () = Int32_binary.barf x.num_want in
    UInt16_binary.barf x.port
end

module Scrape_request_payload_binary : Binary with type t = scrape Request.payload = struct
  open Request
  type t = scrape Request.payload
  let size_of _ = 20

  let slurp =
    let** info_hash = Bytes20_binary.slurp in
    succeed @@ Scrape { info_hash = InfoHash info_hash }

  let barf (Scrape { info_hash = (InfoHash info_hash)}) =
    Bytes20_binary.barf info_hash
end

module Mk_request_binary (M : Message_type with type t = request) : Binary with type t = (M.a,request) message = struct
  type t       = (M.a,request) message
  type payload = M.a Request.payload

  module Header_binary = Mk_header_binary(M)

  let payload_parser : (module Binary with type t = payload) = match M.sing with
    | ConnectReq  -> (module Connect_request_payload_binary  : Binary with type t = payload)
    | AnnounceReq -> (module Announce_request_payload_binary : Binary with type t = payload)
    | ScrapeReq   -> (module Scrape_request_payload_binary   : Binary with type t = payload)

  let size_of =
    let module Payload_binary = (val payload_parser) in function
    | Some Request (_,h,p) -> 8 + Header_binary.size_of (Some h) + Payload_binary.size_of (Some p)
    | None                 -> 8 + Header_binary.size_of None     + Payload_binary.size_of None

  let slurp =
    let module Payload_binary = (val payload_parser) in
    let** c_id    = Int64_binary.slurp in
    let** header  = Header_binary.slurp in
    let** payload = Payload_binary.slurp in
    succeed @@ Request (c_id,header,payload)

  let barf (Request (c_id,header,payload)) =
    let module Payload_binary = (val payload_parser) in
    let** () = Int64_binary.barf c_id in
    let** () = Header_binary.barf header in
    Payload_binary.barf payload
end

module Connect_response_payload_binary : Binary with type t = connect Response.payload = struct
  open Response

  type t = connect Response.payload

  let size_of _ = 8

  let slurp =
    let** connection_id = Int64_binary.slurp
    in succeed @@ Connect {connection_id;}

  let barf (Connect x) = Int64_binary.barf x.connection_id
end

module Announce_response_payload_binary : Binary with type t = announce Response.payload = struct
  open Response
  type t = announce Response.payload

  let rec size_of = function
    | Some Announce x -> size_of None + (List.length x.peers * 6)
    | None            -> 12

  let slurp_peer =
    let** ip   = Int32_binary.slurp in
    let** port = UInt16_binary.slurp in
    succeed {ip = IPv4 ip; port;}

  let rec slurp_peers peers =
    let** is_empty = lift is_empty in
    if is_empty
    then succeed @@ List.rev peers
    else
      let** peer = slurp_peer in
      slurp_peers (peer :: peers)

  let slurp =
    let** interval = Int32_binary.slurp in
    let** leechers = Int32_binary.slurp in
    let** seeders  = Int32_binary.slurp in
    let** peers    = slurp_peers [] in
    succeed @@ Announce { interval; leechers; seeders; peers }

  let rec barf_peers = function
    | (peer :: peers) ->
       let (IPv4 ip) = peer.ip in
       let** () = Int32_binary.barf ip in
       let** () = UInt16_binary.barf peer.port in
       barf_peers peers
    | _ -> succeed ()

  let barf (Announce x)=
    let** () = Int32_binary.barf x.interval in
    let** () = Int32_binary.barf x.leechers in
    let** () = Int32_binary.barf x.seeders in
    barf_peers x.peers
end

module Scrape_response_payload_binary : Binary with type t = scrape Response.payload = struct
  open Response
  type t = scrape Response.payload

  let size_of _ = 12

  let slurp =
    let** complete   = Int32_binary.slurp in
    let** downloaded = Int32_binary.slurp in
    let** incomplete = Int32_binary.slurp in
    succeed @@ Scrape { complete; downloaded; incomplete; }

  let barf (Scrape x) =
    let** () = Int32_binary.barf x.complete in
    let** () = Int32_binary.barf x.downloaded in
    Int32_binary.barf x.incomplete
end

module Error_response_payload_binary : Binary with type t = error Response.payload = struct
  open Response
  type t = error Response.payload

  let size_of = function
    | Some (Error x) -> String_binary.size_of (Some x.error)
    | None           -> 0

  let slurp =
    let** error = String_binary.slurp in
    succeed @@ Error { error; }

  let barf (Error x) = String_binary.barf x.error
end

module Mk_response_binary (M : Message_type with type t = response) : Binary with type t = (M.a,response) message = struct
  type t       = (M.a,response) message
  type payload = M.a Response.payload

  module Header_binary = Mk_header_binary(M)

  let payload_parser : (module Binary with type t = payload) = match M.sing with
    | ConnectRes  -> (module Connect_response_payload_binary  : Binary with type t = payload)
    | AnnounceRes -> (module Announce_response_payload_binary : Binary with type t = payload)
    | ScrapeRes   -> (module Scrape_response_payload_binary   : Binary with type t = payload)
    | ErrorRes    -> (module Error_response_payload_binary    : Binary with type t = payload)

  let size_of = let module Payload_binary = (val payload_parser) in function
    | Some Response (h,p) -> Header_binary.size_of (Some h) + Payload_binary.size_of (Some p)
    | None                -> Header_binary.size_of None     + Payload_binary.size_of None

  let slurp =
    let module Payload_binary = (val payload_parser) in
    let** header  = Header_binary.slurp in
    let** payload = Payload_binary.slurp in
    succeed @@ Response (header,payload)

  let barf (Response (header,payload)) =
    let module Payload_binary = (val payload_parser) in
    let** () = Header_binary.barf header in
    Payload_binary.barf payload
end

module Mk_message_binary (M : Message_type) : Binary with type t = (M.a,M.t) message = struct
  type t = (M.a,M.t) message
  let message_binary : (module Binary with type t = t) = match mk_message_type_proof M.sing with
    | Is_request  _m_sing -> (module Mk_request_binary(M))
    | Is_response _m_sing -> (module Mk_response_binary(M))

  let size_of = let module M = (val message_binary) in M.size_of
  let slurp   = let module M = (val message_binary) in M.slurp
  let barf    = let module M = (val message_binary) in M.barf
end
