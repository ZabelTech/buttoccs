type uint16        = Unsigned.UInt16.t
type ipv4          = IPv4 of int32
type connection_id = int64

(* I *think* this is a hack.
   Somehow the compiler cannot distingush types without a data-constructor.
   That's why I give it a private one, such that it will be able to do so.
   I assume there is a better way though.
 *)
type request  = private A
type response = private C
type announce = private A
type connect  = private C
type scrape   = private S
type error    = private E

type peer      = { ip : ipv4; port : uint16 }
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
                         port       : uint16; }

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

(* note that `error request message is` not possible *)
type (_,_) message_sing =
  | ConnectReq  : (connect, request)  message_sing
  | AnnounceReq : (announce,request)  message_sing
  | ScrapeReq   : (scrape,  request)  message_sing
  | ConnectRes  : (connect, response) message_sing
  | AnnounceRes : (announce,response) message_sing
  | ScrapeRes   : (scrape,  response) message_sing
  | ErrorRes    : (error,   response) message_sing

type (_,_) message =
  | Request  : connection_id * 'a header * 'a Request.payload  -> ('a,request) message
  | Response :                 'a header * 'a Response.payload -> ('a,response) message

exception NoErrorReq

let get_action_sing : type a b. (a,b) message_sing -> a action = function
  | ConnectReq  -> Connect
  | AnnounceReq -> Announce
  | ScrapeReq   -> Scrape
  | ConnectRes  -> Connect
  | AnnounceRes -> Announce
  | ScrapeRes   -> Scrape
  | ErrorRes    -> Error

let get_message_sing : type a b. (a,b) message -> (a,b) message_sing = function
  | (Request (_,{action = Connect;  _},_)) -> ConnectReq
  | (Request (_,{action = Announce; _},_)) -> AnnounceReq
  | (Request (_,{action = Scrape;   _},_)) -> ScrapeReq
  | (Request (_,{action = Error;   _}, _)) -> . (* an error request is impossible *)
  | (Response ({action = Connect;  _},_)) -> ConnectRes
  | (Response ({action = Announce; _},_)) -> AnnounceRes
  | (Response ({action = Scrape;   _},_)) -> ScrapeRes
  | (Response ({action = Error;   _},_))  -> ErrorRes
