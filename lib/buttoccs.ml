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
  type announce_payload = { info_hash  : info_hash;
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

  type scrape_payload = { info_hash : info_hash }

  type _ payload =
    | Announce : announce_payload -> announce payload
    | Scrape   : scrape_payload   -> scrape payload
    | Connect  : connect payload
end

module Response = struct
  type peer     = { ip : ipv4; port : uint16 }
  type error_payload    = { error : string }
  type connect_payload  = { connection_id : connection_id }
  type announce_payload = { interval : int32;
                            leechers : int32;
                            seeders  : int32;
                            peers    : peer list; }

  type scrape_payload   = { complete   : int32;
                            downloaded : int32;
                            incomplete : int32; }

  type _ payload =
    | Announce : announce_payload -> announce payload
    | Scrape   : scrape_payload   -> scrape payload
    | Connect  : connect_payload  -> connect payload
    | Error    : error_payload    -> error payload
end

type 'a request  = Request  of connection_id * 'a header * 'a Request.payload
type 'a response = Response of 'a header * 'a Response.payload


type parse_error = NotEnoughData
                 | UnexpectedAction

module type Binary = sig
  type t
  val min_size  : int
  val calc_size : t -> int
  val slurp     : bytes -> ((int * t),parse_error) result
  val barf      : t -> bytes -> unit
end

let fmap = Result.map
let ensure_size s b cont = if Bytes.length b < s then Result.Error NotEnoughData else cont

let (let*) o f =
  match o with
  | Ok r    -> f r
  | Error e -> Result.Error e

module type Message_type = sig
  type t
  module Action   : Binary with type t := t action
  module Request  : Binary with type t := t Request.payload
  module Response : Binary with type t := t Response.payload
end

module Connect : Message_type with type t = connect = struct
  type t = connect

  module Action : Binary with type t = connect action = struct
    type t       = connect action
    let min_size = 4
    let slurp b = ensure_size min_size b @@
      if (Bytes.get_int32_be b 4) = Int32.zero
      then Ok (4,Connect)
      else Result.Error UnexpectedAction
    let barf  = undefined
    let calc_size = undefined
  end

  module Request : Binary with type t = connect Request.payload = struct
    type t       = connect Request.payload
    let min_size = 4
    let slurp _  = Ok (0,Request.Connect)
    let barf  _  = undefined (* Bytes.empty *)
    let calc_size = undefined
  end

  module Response : Binary with type t = connect Response.payload = struct
    type t       = connect Response.payload
    let min_size = 8
    let slurp b  = ensure_size min_size b @@
      Ok (8,Response.Connect {connection_id = Bytes.get_int64_be b 0})
    let barf     = undefined
    let calc_size = undefined
  end
end

module Announce : Message_type with type t = announce = struct
  type t   = announce

  module Action : Binary with type t = announce action = struct
    type t       = announce action
    let min_size = 4
    let slurp b = ensure_size min_size b @@
      if (Bytes.get_int32_be b 4) = Int32.of_int 1
      then Ok (4,Announce)
      else Result.Error UnexpectedAction
    let barf  = undefined
    let calc_size = undefined
  end

  module Request : Binary with type t = announce Request.payload = struct
    type t       = announce Request.payload
    let min_size = 4
    let slurp _  = undefined
    let barf  _  = undefined
    let calc_size = undefined
  end

  module Response : Binary with type t = announce Response.payload = struct
    type t       = announce Response.payload
    let min_size = 8
    let slurp _b = undefined
    let barf     = undefined
    let calc_size = undefined
  end
end

module Binary_header (M : Message_type) : Binary with type t = M.t header = struct
  type t       = M.t header
  let min_size = 8
  let slurp b = ensure_size min_size b @@
    let* (cnt,action)   = M.Action.slurp (Bytes.sub b 0 undefined) in
    let  transaction_id = Bytes.get_int32_be b 4 in
    Ok (cnt + 4,{ action = action; transaction_id = transaction_id; })

  let barf  = undefined
  let calc_size = undefined
end

module Binary_request (M : Message_type) : Binary with type t = M.t request = struct
  type t       = M.t request
  module H     = Binary_header(M)
  let min_size = 8 + H.min_size + M.Request.min_size
  let slurp b =
    ensure_size min_size b @@
      let connection_id   = Bytes.get_int64_be b 0 in
      let* (cnt1,header)  = H.slurp (Bytes.sub b 1 undefined) in
      let* (cnt2,payload) = M.Request.slurp (Bytes.sub b 1 undefined) in
      Ok (8 + cnt1 + cnt2,(Request (connection_id,header,payload)))

  let barf  _ = undefined
  let calc_size = undefined
end

module Binary_response (M : Message_type) : Binary with type t = M.t response = struct
  type t   = M.t response
  module H = Binary_header(M)
  let min_size  = H.min_size + M.Response.min_size
  let slurp b =
    ensure_size min_size b @@
      let* (cnt1,header)  = H.slurp (Bytes.sub b 1 undefined) in
      let* (cnt2,payload) = M.Response.slurp (Bytes.sub b 1 undefined) in
      Ok (cnt1+cnt2,(Response (header,payload)))

  let barf  _ = undefined
  let calc_size = undefined
end
