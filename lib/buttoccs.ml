type ipv4          = IPv4 of int32
type uint16        = UINT16
type connection_id = int64

let undefined = assert false

type announce
type connect
type scrape
type error

type _ action =
  | Announce : announce action
  | Connect  : connect action
  | Scrape   : scrape action
  | Error    : error action

type 'a header = {
    action        : 'a action;
    transaction_id: int32;
  }

module Request = struct
  type event = None
             | Completed
             | Started
             | Stopped
  type info_hash = InfoHash of bytes  (* [20] *)
  type peer_id   = PeerId of bytes  (* [20] *)
  type announce  = { info_hash  : info_hash;
                     peer_id    : peer_id;
                     downloaded : int64;
                     left       : int64;
                     uploaded   : int64;
                     event      : event;
                     ip         : ipv4;
                     key        : int32;
                     num_want   : int32;
                     port       : uint16;
                   }

  type scrape = { info_hash : info_hash }

  type _ payload =
    | Announce : announce -> announce payload
    | Scrape   : scrape   -> scrape payload
    | Connect  : 'connect payload
end

module Response = struct
  type peer     = { ip : ipv4; port : uint16 }
  type error    = { error : string }
  type connect  = { connection_id : connection_id }
  type announce = { interval : int32;
                    leechers : int32;
                    seeders  : int32;
                    peers    : peer list; }

  type scrape   = { complete   : int32;
                    downloaded : int32;
                    incomplete : int32; }

  type _ payload =
    | Announce : announce -> announce payload
    | Scrape   : scrape   -> scrape payload
    | Connect  : connect  -> connect payload
    | Error    : error    -> connect payload
end

type 'a request  = Request  of connection_id * 'a header * 'a Request.payload
type 'a response = Response of 'a header * 'a Response.payload


type parse_error = NotEnoughData
                 | UnexpectedAction

module type Binary = sig
  type t
  val size  : int
  val slurp : bytes -> (t,parse_error) result
  val barf  : t -> bytes
end

let fmap = Result.map
let ensure_size s b cont = if Bytes.length b < s then Result.Error NotEnoughData else cont

let (let*) o f =
  match o with
  | Ok r    -> Ok (f r)
  | Error e -> Result.Error e

module type ActionType = sig
  type t
  include Binary with type t := t action
  val  s : t action
end

module ConnectAction : ActionType with type t = connect = struct
  type t = connect
  let s  = Connect
  let size = 4
  let slurp b = ensure_size size b @@
    if (Bytes.get_int32_be b 4) = Int32.zero
    then Ok s
    else Result.Error UnexpectedAction
  let barf  = undefined
end

module AnnounceAction : ActionType with type t = announce = struct
  type t = announce
  let s  = Announce
  let size = 4
  let slurp b = ensure_size size b @@
    if (Bytes.get_int32_be b 4) = undefined
    then Ok s
    else Result.Error UnexpectedAction
  let barf  = undefined
end

module Binary_header (A : ActionType) : Binary with type t = A.t header = struct
  type t   = A.t header
  let size = 8
  let slurp b = ensure_size size b @@
    let* _action = A.slurp (Bytes.sub b 0 4)
    in { action = A.s; transaction_id = Bytes.get_int32_be b 4; }

  let barf  = undefined
end

module Binary_connect_request (A : ActionType) : Binary with type t = A.t request = struct
  type t   = A.t request
  let size = 16
  let slurp b =
    let open(Binary_header(A)) in
    ensure_size size b @@
    let* header = slurp (Bytes.sub b 1 A.size)
    in Request (Bytes.get_int64_be b 0 ,header,Connect)

  let barf  _ = undefined
end
