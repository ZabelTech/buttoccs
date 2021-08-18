type ipv4          = IPv4 of int32
type connection_id = int64

(* I *think* this is a hack.
   Somehow the compiler cannot distingush types without a data-constructor.
   That's why I give it a private one, such that it will be able to do so.
   I assume there is a better way though.
 *)
type announce = private A
type connect  = private C
type scrape   = private S
type error    = private E

type peer      = { ip : ipv4; port : int }
type info_hash = InfoHash of bytes  (* [20] *)
type peer_id   = PeerId of bytes  (* [20] *)

type event = None
           | Completed
           | Started
           | Stopped

type _ action = | Announce : announce action
                | Connect  : connect action
                | Scrape   : scrape action
                | Error    : error action

type 'a header = { action        : 'a action;
                   transaction_id: int32; }

module Request = struct
  type announce_data = { info_hash  : info_hash;
                         peer_id    : peer_id;
                         downloaded : int64;
                         left       : int64;
                         uploaded   : int64;
                         event      : event;
                         ip         : ipv4;
                         key        : int32;
                         num_want   : int32;
                         port       : int; }

  type scrape_data = { info_hash : info_hash }

  type _ payload =
    | Announce : announce_data -> announce payload
    | Scrape   : scrape_data   -> scrape payload
    | Connect  : connect payload
end

module Response = struct
  type error_data    = { error : string }

  type connect_data  = { connection_id : connection_id }

  type announce_data = { interval : int32;
                         leechers : int32;
                         seeders  : int32;
                         peers    : peer list; }

  type scrape_data = { complete   : int32;
                       downloaded : int32;
                       incomplete : int32; }

  type _ payload =
    | Announce : announce_data -> announce payload
    | Scrape   : scrape_data   -> scrape payload
    | Connect  : connect_data  -> connect payload
    | Error    : error_data    -> error payload
end

type 'a request  = Request  of connection_id * 'a header * 'a Request.payload
type 'a response = Response of 'a header * 'a Response.payload
