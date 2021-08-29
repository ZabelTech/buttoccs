open Buttoccs
open Message
open Message_binary
open Pretty
open QCheck
open Gen

let ( let* ) o f = o >>= f

let (<$>) x fn = map fn x

let gen_string = string
let gen_i64    = ui64
let gen_i32    = ui32
let gen_ui16   = map Unsigned.UInt16.of_int @@ int_range 0 65536 (* 2 Byte *)

let gen_bytes n s =
  let gen_byte _ = Char.chr @@ Gen.int_range 0 255 s in
  Bytes.init n gen_byte

let gen_header a =
  gen_i32 <$> fun transaction_id -> { action = a; transaction_id; }

let gen_info_hash = gen_bytes 20 <$> fun bs -> InfoHash bs
let gen_peer_id   = gen_bytes 20 <$> fun bs -> PeerId bs

let gen_event = oneofl [None; Completed; Started; Stopped]

let gen_announce_req_payload =
  let* info_hash  = gen_info_hash in
  let* peer_id    = gen_peer_id in
  let* downloaded = gen_i64 in
  let* left       = gen_i64 in
  let* uploaded   = gen_i64 in
  let* event      = gen_event in
  let* ip         = gen_i32 in
  let* key        = gen_i32 in
  let* num_want   = gen_i32 in
  let* port       = gen_ui16 in
  return @@ Request.Announce { ip = IPv4 ip;
                               info_hash; peer_id; downloaded; left;
                               uploaded; event; key; num_want; port; }

let gen_announce_res_payload =
  let* interval = gen_i32 in
  let* leechers = gen_i32 in
  let* seeders  = gen_i32 in
  let gen_peer =
    let* ip   = gen_i32 in
    let* port = gen_ui16 in
    return { ip = IPv4 ip; port = port } in
  let* peers = list gen_peer in
  return @@ Response.Announce { interval; leechers; seeders; peers; }

let gen_scrape_res_payload =
  let* complete   = gen_i32 in
  let* downloaded = gen_i32 in
  let* incomplete = gen_i32 in
  return @@ Response.Scrape { complete; downloaded; incomplete; }

let gen_req_payload : type a. (a,request) message_sing -> a Request.payload Gen.t = function
  | ConnectReq  -> return Request.Connect
  | AnnounceReq -> gen_announce_req_payload
  | ScrapeReq   -> gen_info_hash <$> fun info_hash -> Request.Scrape { info_hash; }

let gen_res_payload : type a. (a,response) message_sing -> a Response.payload Gen.t = function
  | ConnectRes  -> gen_i64 <$> fun connection_id -> Response.Connect { connection_id; }
  | AnnounceRes -> gen_announce_res_payload
  | ScrapeRes   -> gen_scrape_res_payload
  | ErrorRes    -> gen_string <$> fun error -> Response.Error { error; }

let gen_message : type a b. (a,b) message_type_proof -> (a,b) message Gen.t  = function
  | (Is_request req) ->
     let* c_id    = gen_i64 in
     let* header  = gen_header (get_action_sing req) in
     let* payload = gen_req_payload req
     in return @@ Request(c_id,header,payload)

  | (Is_response res) ->
     let* header  = gen_header (get_action_sing res) in
     let* payload = gen_res_payload res
     in return @@ Response(header,payload)

let arb_message m_sing =
  make
    ~print:(pprint_message)
    (gen_message @@ mk_message_type_proof m_sing)

exception TestException of string

let message_binary_test (type action) (type ty) m_sing =
  let m = to_message_type m_sing in
  let module M = (val m : Message_type with type t = ty and type a = action) in
  let module B = Mk_message_binary(M) in
  QCheck.Test.make ~count:1000 (arb_message m_sing) (fun req ->
      let size   = B.size_of (Some req) in
      let bs     = Bytes.create size in
      let _      = B.barf req bs 0 in
      let (_size,result) = B.slurp bs 0 in
      match result with
      | Ok r when r = req -> true
      | Ok r    -> raise(TestException (String.concat "\n" [pprint_message req; pprint_message r]))
      | Error r -> raise(TestException (pprint_binary_error r))
    )

let qcheck_tests = [
    message_binary_test ConnectReq
  ; message_binary_test ConnectRes
  ; message_binary_test AnnounceRes
  ; message_binary_test AnnounceReq
  ; message_binary_test ScrapeReq
  ; message_binary_test ScrapeRes
  ; message_binary_test ErrorRes ]

let suite =
  let open OUnit in
  "Binary Idempotency " >::: List.map (QCheck_ounit.to_ounit_test) qcheck_tests

let _ =
  QCheck_runner.run_tests
          ~colors:true
          ~verbose:true
          ~long:true
          qcheck_tests
