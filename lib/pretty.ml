open Binary
open Message_binary
open Message
open Printf

let pprint_ip (IPv4 ip)            = sprintf "%li" ip
let pprint_info_hash (InfoHash _h) = sprintf "<..>"
let pprint_peer_id (PeerId _p)     = sprintf "<..>"
let pprint_port port               = sprintf "%i" (Unsigned.UInt16.to_int port)

let pprint_event = function
  | None      -> "None"
  | Completed -> "Completed"
  | Started   -> "Started"
  | Stopped   -> "Stopped"

let pprint_action : type a. a action -> string = function
  | Connect  -> "Connect"
  | Announce -> "Announce"
  | Scrape   -> "Scrape"
  | Error    -> "Error"

let pprint_peers peers =
  let pprint_peer x = sprintf "{ ip = %s, port = %s }" (pprint_ip x.ip) (pprint_port x.port)
  in String.concat "," (List.map pprint_peer peers)

let pprint_header h = sprintf "{ action: %s, transaction_id: %li" (pprint_action h.action) h.transaction_id

let pprint_req_payload : type a. a Request.payload -> string = function
  | Connect      -> "Connect <empty>"
  | (Announce x) ->
      sprintf "Announce { info_hash = %s
           , peer_id   = %s
           , downloaded = %Li
           , left       = %Li
           , uploaded   = %Li
           , event      = %s
           , ip         = %s
           , key        = %li
           , num_want   = %li
           , port       = %s }"
      (pprint_info_hash x.info_hash) (pprint_peer_id x.peer_id) x.downloaded x.left
      x.uploaded (pprint_event x.event) (pprint_ip x.ip) x.key x.num_want (pprint_port x.port)
  | (Scrape x) ->
      sprintf "Scrape { info_hash = %s } "  (pprint_info_hash x.info_hash)

let pprint_res_payload : type a. a Response.payload -> string = function
  | (Connect x)  -> sprintf "Connect { connection_id = %Li" x.connection_id
  | (Announce x) ->
      sprintf "Announce { interval = %li
           , leechers = %li
           , seeders  = %li
           , peers    = %s }"
      x.interval x.leechers x.seeders (pprint_peers x.peers)
  | (Scrape x) ->
      sprintf "Scrape { complete = %li
           , downloaded = %li
           , incomplete = %li }"
      x.complete x.downloaded x.incomplete
  | (Error x) ->
      sprintf "Error { error = %s } "  x.error

let pprint_message : type a b. (a,b) message -> string = function
  | (Request (c_id,h,p)) -> sprintf "Request { connection_id: %Li, header: %s, payload = %s }" c_id (pprint_header h) (pprint_req_payload p)
  | (Response (h,p))     -> sprintf "Response { header: %s, payload = %s }" (pprint_header h) (pprint_res_payload p)

let pprint_binary_error = function
  | Not_enough_bytes    -> "Not_enough_bytes"
  | Unknown_event _     -> "Unknown_event"
  | Unexpected_action _ -> "Unexpected_action"
  | _ -> "<unknown>"
