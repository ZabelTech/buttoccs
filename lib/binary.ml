open Bytes
open Result
open Message

type parse_error = NotEnoughData
                 | UnexpectedAction
                 | UnknownEvent of int

let get_sized size fn bs offset =
  if length bs - offset < size then error NotEnoughData else ok (offset + size, fn bs offset)

let put_sized size fn bs offset x = let () = fn bs offset x in offset + size

(* TODO use smth like this this for rws monad
 * let (>>=) gen f st =
 *   f (gen st) st *)

let (let*) o f =
  match o with
  | Ok r    -> f r
  | Error e -> Result.Error e

module type Binary = sig
  type t
  val calc_size : t -> int
  val min_size  : int
  val slurp : bytes -> int -> (int * t,parse_error) result
  val barf  : bytes -> int -> t -> int
end

module type Some_action = sig
  type t
  val constructor : t action
  val idx : int
end

module type Some_request = sig
  type t
  module Action   : Some_action with type t := t
  module Header   : Binary with type t     := t header
  module Payload  : Binary with type t     := t Request.payload
end

module type Some_response = sig
  type t
  module Action   : Some_action with type t := t
  module Header   : Binary with type t     := t header
  module Payload  : Binary with type t     := t Response.payload
end

module Int64_binary : Binary with type t := int64 = struct
  let min_size    = 8
  let calc_size _ = 8
  let slurp = get_sized 8 get_int64_be
  let barf  = put_sized 8 set_int64_be
end

module Int32_binary : Binary with type t := int32 = struct
  let min_size    = 4
  let calc_size _ = 4
  let slurp = get_sized 4 get_int32_be
  let barf  = put_sized 4 set_int32_be
end

module UInt16_binary : Binary with type t := uint16 = struct
  let min_size    = 2
  let calc_size _ = 2
  let slurp = get_sized 2 (fun bs offset -> Unsigned.UInt16.of_int @@ get_uint16_be bs offset)
  let barf  = put_sized 2 (fun bs offset x -> set_uint16_be bs offset @@ Unsigned.UInt16.to_int x)
end

module Bytes20_binary : Binary with type t := bytes = struct
  let min_size    = 20
  let calc_size _ = 20
  let slurp = get_sized 20 (fun bs offset -> sub bs offset 20)
  let barf  = put_sized 20 (fun bs offset x -> blit x 0 bs offset 20)
end

module String_binary : Binary with type t := string = struct
  let min_size  = 0
  let calc_size = String.length
  let slurp bs offset =
    let rest_length = length bs - offset in
    get_sized rest_length (fun _ _ -> sub_string bs offset rest_length) bs offset

  let barf bs offset x =
    let len = String.length x in
    put_sized len (fun _ _ _ -> blit_string x 0 bs offset len) bs offset x
end

module Event_binary : Binary with type t := event = struct
  let min_size    = 4
  let calc_size _ = 4
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

module Connect_action : Some_action with type t = connect = struct
  type t          = connect
  let idx         = 0
  let constructor = Connect
end

module Announce_action : Some_action with type t = announce = struct
  type t          = announce
  let idx         = 0
  let constructor = Announce
end

module Scrape_action : Some_action with type t = scrape = struct
  type t          = scrape
  let idx         = 0
  let constructor = Scrape
end

module Error_action : Some_action with type t = error = struct
  type t          = error
  let idx         = 0
  let constructor = Error
end

(* Not ideal.
   I don't want a binary instance per gadt constructor here.
   Instead I have a Functor and a module-type that serves as its argument,
   to create an instance for each constructor on the fly.
 *)

module Action_binary (A : Some_action) : Binary with type t := A.t action = struct
  let min_size    = 4
  let calc_size _ = 4
  let slurp bs offset =
    let* (offset,res) = Int32_binary.slurp bs offset
    in if Int32.to_int res = A.idx
       then ok (offset,A.constructor)
       else error UnexpectedAction

  let barf bs offset _ = Int32_binary.barf bs offset @@ Int32.of_int @@ A.idx
end

module Binary_header (A : Some_action) : Binary with type t := A.t header = struct
  module B = Action_binary(A)
  let min_size    = 8
  let calc_size _ = 8

  let slurp bs offset =
    let* (offset,action)         = B.slurp bs offset in
    let* (offset,transaction_id) = Int32_binary.slurp bs offset in
    ok (offset,{ action = action; transaction_id = transaction_id; })

  let barf bs offset x =
    let offset = B.barf bs offset x.action in
    Int32_binary.barf bs offset x.transaction_id
end

module Binary_request (R : Some_request) : Binary with type t = (R.t,request) message = struct
  type t = (R.t,request) message
  let min_size                    = 8 + R.Header.min_size + R.Payload.min_size
  let calc_size (Request (_,h,p)) = 8 + R.Header.calc_size h + R.Payload.calc_size p

  let slurp bs offset =
    let* (offset,connection_id) = Int64_binary.slurp bs offset in
    let* (offset,header)        = R.Header.slurp bs offset in
    let* (offset,payload)       = R.Payload.slurp bs offset in
    Ok (offset,Request (connection_id,header,payload))

  let barf bs offset (Request (connection_id,header,payload)) =
    let offset = Int64_binary.barf bs offset connection_id in
    let offset = R.Header.barf bs offset header in
    R.Payload.barf bs offset payload
end

module Binary_response (R : Some_response) : Binary with type t = (R.t,response) message = struct
  type t = (R.t,response) message
  let min_size                   = R.Header.min_size + R.Payload.min_size
  let calc_size (Response (h,p)) = R.Header.calc_size h + R.Payload.calc_size p

  let slurp bs offset =
    let* (offset,header)  = R.Header.slurp bs offset in
    let* (offset,payload) = R.Payload.slurp bs offset in
    Ok (offset,(Response (header,payload)))

  let barf bs offset (Response (header,payload)) =
    let offset = R.Header.barf bs offset header in
    R.Payload.barf bs offset payload
end

module Connect_request : Some_request with type t = connect = struct
  type t        = connect
  module Action = Connect_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t = connect Request.payload= struct
    open Request
    type t = connect Request.payload
    let min_size    = 0
    let calc_size _ = 0
    let slurp _ offset   = ok (offset,Connect)
    let barf  _ offset _ = offset
  end
end

module Announce_request : Some_request with type t = announce = struct
  type t        = announce
  module Action = Announce_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := announce Request.payload = struct
    open Request
    let min_size    = 82
    let calc_size _ = 82

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
end

module Scrape_request : Some_request with type t = scrape = struct
  type t        = scrape
  module Action = Scrape_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := scrape Request.payload = struct
    open Request
    let min_size    = 20
    let calc_size _ = 20

    let slurp bs offset =
      let* (offset,info_hash) = Bytes20_binary.slurp bs offset in
      ok (offset,Scrape { info_hash = InfoHash info_hash })

    let barf bs offset (Scrape x) =
      let (InfoHash info_hash) = x.info_hash in
      Bytes20_binary.barf bs offset info_hash
  end
end

module Connect_response : Some_response with type t = connect = struct
  type t        = connect
  module Action = Connect_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := connect Response.payload = struct
    open Response
    let min_size    = 8
    let calc_size _ = 8

    let slurp bs offset =
      let* (offset,connection_id) = Int64_binary.slurp bs offset
      in ok (offset, Connect {connection_id = connection_id})

    let barf bs offset (Connect x) = Int64_binary.barf bs offset x.connection_id
  end
end

module Announce_response : Some_response with type t = announce = struct
  type t        = announce
  module Action = Announce_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := announce Response.payload = struct
    open Response
    let min_size = 12
    let calc_size (Announce x) = min_size + (List.length x.peers * 6)

    let rec slurpPeers peers bs = function
      | offset when offset >= length bs -> ok (offset,(List.rev peers))
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
end

module Scrape_response : Some_response with type t = scrape = struct
  type t        = scrape
  module Action = Scrape_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := scrape Response.payload = struct
    open Response
    let min_size    = 12
    let calc_size _ = 12

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
end

module Error_response : Some_response with type t = error = struct
  type t        = error
  module Action = Error_action
  module Header = Binary_header(Action)

  module Payload : Binary with type t := error Response.payload = struct
    open Response
    let min_size = 0
    let calc_size (Error x) = String_binary.calc_size x.error

    let slurp bs offset =
      let* (offset,str) = String_binary.slurp bs offset in
      ok (offset,Error { error = str })

    let barf bs offset (Error x) = String_binary.barf bs offset x.error
  end
end

let get_binary_message : type a b. (a,b) message_sing -> (module Binary with type t = (a,b) message) = function
  | ConnectReq  -> (module Binary_request(Connect_request))
  | AnnounceReq -> (module Binary_request(Announce_request))
  | ScrapeReq   -> (module Binary_request(Scrape_request))
  | ConnectRes  -> (module Binary_response(Connect_response))
  | AnnounceRes -> (module Binary_response(Announce_response))
  | ScrapeRes   -> (module Binary_response(Scrape_response))
  | ErrorRes    -> (module Binary_response(Error_response))

let parse_message (type a) (type b) (bs : bytes) (message_sing : (a,b) message_sing) =
  let m = get_binary_message message_sing in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.slurp bs 0

let build_message (type a) (type b) (bs : bytes) (message : (a,b) message) =
  let m = get_binary_message (get_message_sing message) in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.barf bs 0 message

let min_size (type a) (type b) (message_sing : (a,b) message_sing) =
  let m = get_binary_message message_sing in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.min_size

let calc_size (type a) (type b) (message : (a,b) message) =
  let m = get_binary_message (get_message_sing message) in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.calc_size message
