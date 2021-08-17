type ipv4          = IPv4 of int32
type uint16        = UINT16
type connection_id = int64

type announce
type connect
type scrape
type error_msg

type _ action =
  | Announce : announce action
  | Connect  : connect action
  | Scrape   : scrape action
  | ErrorMsg : error_msg action

type 'a header = {
    action        : 'a action;
    transaction_id: int32;
  }

type event = None
           | Completed
           | Started
           | Stopped

type info_hash = InfoHash of bytes  (* [20] *)

type peer_id   = PeerId of bytes  (* [20] *)

module Request = struct
  type announce_payload = { info_hash  : info_hash;
                            peer_id    : peer_id;
                            downloaded : int64;
                            left       : int64;
                            uploaded   : int64;
                            event      : event;
                            ip         : ipv4;
                            key        : int32;
                            num_want   : int32;
                            port       : int;
                          }

  type scrape_payload = { info_hash : info_hash }

  type _ payload =
    | Announce : announce_payload -> announce payload
    | Scrape   : scrape_payload   -> scrape payload
    | Connect  : connect payload
end

type peer     = { ip : ipv4; port : int }

module Response = struct
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
    | ErrorMsg : error_payload    -> error_msg payload
end

type 'a request  = Request  of connection_id * 'a header * 'a Request.payload
type 'a response = Response of 'a header * 'a Response.payload
