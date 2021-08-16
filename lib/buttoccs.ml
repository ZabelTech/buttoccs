type ipv4          = IPv4 of int32
type uint16        = UINT16
type connection_id = int64

type _ action =
  | Announce : 'announce action
  | Connect  : 'connect action
  | Scrape   : 'scrape action
  | Error    : 'error action

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

type 'a request  = Request of connection_id * 'a header * 'a Request.payload
type 'a response = Request of 'a header * 'a Response.payload
