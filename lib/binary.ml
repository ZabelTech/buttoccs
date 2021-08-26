open Bytes
open Result
open Message

type parse_error = NotEnoughData
                 | UnexpectedAction
                 | UnknownEvent of int

type byte_offset     = int

(* A poor mans ad-hoc Monad-stack implementation.
   Run with this for now. It's definitly better than the prior status quo i.e. explicit parameter passing.
   Maybe reconsider this judgement in the future,
   as it might be more idiomatic to wrap bytes+offset in an object.
 *)

type 'a monad_binary = bytes -> byte_offset -> ((byte_offset * 'a),parse_error) result

let (>>=) : 'a monad_binary -> ('a -> 'b monad_binary) -> 'b monad_binary = fun o fn bytes offset ->
  match o bytes offset with
  | Ok (new_offset,return_value) -> fn return_value bytes new_offset
  | Error e -> error e

let (let*) o f = o >>= f

let return : type a. a -> a monad_binary  = fun x _ s -> )
let fail : parse_error -> 'a monad_binary = fun e _ _ -> error e
let is_empty : bool monad_binary = fun bs offset ->
  if length bs <= offset then ok (offset,true) else ok (offset,false)

let get_sized size fn bs offset =
  if length bs - offset < size then error NotEnoughData else ok (offset + size, fn bs offset)

let put_sized size fn x bs offset =
  ok (offset + size,fn bs offset x)

module type Binary = sig
  type t
  val calc_size : t -> int
  val min_size  : int
  val slurp : t monad_binary
  val barf  : t -> unit monad_binary
end

module type Some_action = sig
  type t
  val constructor : t action
  val idx : int
end

module type Some_request = sig
  type t
  module Action   : Some_action with type t := t
  module Header   : Binary with type t      := t header
  module Payload  : Binary with type t      := t Request.payload
end

module type Some_response = sig
  type t
  module Action   : Some_action with type t := t
  module Header   : Binary with type t      := t header
  module Payload  : Binary with type t      := t Response.payload
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

  let barf x bs offset =
    let len = String.length x in
    put_sized len (fun _ _ _ -> blit_string x 0 bs offset len) x bs offset
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

  let slurp =
    let* eventIdx = Int32_binary.slurp in
    match int2Event @@ Int32.to_int eventIdx with
    | Error e -> fail e
    | Ok r    -> return r

  let barf i = Int32_binary.barf @@ Int32.of_int @@ event2Int i
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
  let slurp =
    let* res = Int32_binary.slurp
    in if Int32.to_int res = A.idx
       then return A.constructor
       else fail UnexpectedAction

  let barf _ = Int32_binary.barf @@ Int32.of_int @@ A.idx
end

module Binary_header (A : Some_action) : Binary with type t := A.t header = struct
  module B = Action_binary(A)
  let min_size    = 8
  let calc_size _ = 8

  let slurp =
    let* action         = B.slurp in
    let* transaction_id = Int32_binary.slurp in
    return { action = action; transaction_id = transaction_id; }

  let barf x =
    let* () = B.barf x.action in
    Int32_binary.barf x.transaction_id
end

module Binary_request (R : Some_request) : Binary with type t = (R.t,request) message = struct
  type t = (R.t,request) message
  let min_size                    = 8 + R.Header.min_size + R.Payload.min_size
  let calc_size (Request (_,h,p)) = 8 + R.Header.calc_size h + R.Payload.calc_size p

  let slurp =
    let* connection_id = Int64_binary.slurp in
    let* header        = R.Header.slurp in
    let* payload       = R.Payload.slurp in
    return @@ Request (connection_id,header,payload)

  let barf (Request (connection_id,header,payload)) =
    let* () = Int64_binary.barf connection_id in
    let* () = R.Header.barf header in
    R.Payload.barf payload
end

module Binary_response (R : Some_response) : Binary with type t = (R.t,response) message = struct
  type t = (R.t,response) message
  let min_size                   = R.Header.min_size + R.Payload.min_size
  let calc_size (Response (h,p)) = R.Header.calc_size h + R.Payload.calc_size p

  let slurp =
    let* header  = R.Header.slurp in
    let* payload = R.Payload.slurp in
    return @@ Response (header,payload)

  let barf (Response (header,payload)) =
    let* () = R.Header.barf header in
    R.Payload.barf payload
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
    let slurp       = return Connect
    let barf _      = return ()
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

    let slurp =
      let* info_hash  = Bytes20_binary.slurp in
      let* peer_id    = Bytes20_binary.slurp in
      let* downloaded = Int64_binary.slurp in
      let* left       = Int64_binary.slurp in
      let* uploaded   = Int64_binary.slurp in
      let* event      = Event_binary.slurp in
      let* ip         = Int32_binary.slurp in
      let* key        = Int32_binary.slurp in
      let* num_want   = Int32_binary.slurp in
      let* port       = UInt16_binary.slurp in
      return @@ Announce { info_hash  = InfoHash info_hash;
                             peer_id    = PeerId peer_id;
                             downloaded = downloaded;
                             left       = left;
                             uploaded   = uploaded;
                             event      = event;
                             ip         = IPv4 ip;
                             key        = key;
                             num_want   = num_want;
                             port       = port; }

    let barf (Announce x) =
      let (InfoHash info_hash) = x.info_hash in
      let (PeerId peer_id)     = x.peer_id in
      let (IPv4 ip)            = x.ip in
      let* () = Bytes20_binary.barf info_hash in
      let* () = Bytes20_binary.barf peer_id in
      let* () = Int64_binary.barf x.downloaded in
      let* () = Int64_binary.barf x.left in
      let* () = Int64_binary.barf x.uploaded in
      let* () = Event_binary.barf x.event in
      let* () = Int32_binary.barf ip in
      let* () = Int32_binary.barf x.key in
      let* () = Int32_binary.barf x.num_want in
      UInt16_binary.barf x.port
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

    let slurp =
      let* info_hash = Bytes20_binary.slurp in
      return @@ Scrape { info_hash = InfoHash info_hash }

    let barf (Scrape x) =
      let (InfoHash info_hash) = x.info_hash in
      Bytes20_binary.barf info_hash
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

    let slurp =
      let* connection_id = Int64_binary.slurp
      in return @@ Connect {connection_id = connection_id}

    let barf (Connect x) = Int64_binary.barf x.connection_id
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

    let slurp_peer =
      let* ip   = Int32_binary.slurp in
      let* port = UInt16_binary.slurp in
      return {ip = IPv4 ip; port = port}

    let rec slurp_peers peers =
      let* is_empty = is_empty in
      if is_empty
      then return @@ List.rev peers
      else
         let* peer = slurp_peer in
         slurp_peers (peer :: peers)

    let slurp =
      let* interval = Int32_binary.slurp in
      let* leechers = Int32_binary.slurp in
      let* seeders  = Int32_binary.slurp in
      let* peers    = slurp_peers [] in
      return @@ Announce { interval = interval;
                           leechers = leechers;
                           seeders  = seeders;
                           peers    = peers }

    let rec barf_peers = function
      | (peer :: peers) ->
         let (IPv4 ip) = peer.ip in
         let* () = Int32_binary.barf ip in
         let* () = UInt16_binary.barf peer.port in
         barf_peers peers
      | _ -> return ()

    let barf (Announce x)=
      let* () = Int32_binary.barf x.interval in
      let* () = Int32_binary.barf x.leechers in
      let* () = Int32_binary.barf x.seeders in
      barf_peers x.peers
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

    let slurp =
      let* complete   = Int32_binary.slurp in
      let* downloaded = Int32_binary.slurp in
      let* incomplete = Int32_binary.slurp in
      return @@ Scrape { complete   = complete;
                         downloaded = downloaded;
                         incomplete = incomplete; }

    let barf (Scrape x) =
      let* () = Int32_binary.barf x.complete in
      let* () = Int32_binary.barf x.downloaded in
      Int32_binary.barf x.incomplete
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

    let slurp =
      let* str = String_binary.slurp in
      return @@ Error { error = str }

    let barf (Error x) = String_binary.barf x.error
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
  M.barf message bs 0

let min_size (type a) (type b) (message_sing : (a,b) message_sing) =
  let m = get_binary_message message_sing in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.min_size

let calc_size (type a) (type b) (message : (a,b) message) =
  let m = get_binary_message (get_message_sing message) in
  let module M = (val m : Binary with type t = (a,b) message) in
  M.calc_size message
