open Mqtt.Mqtt_packet

(* TODO: this is silly *)
let decode_length s i =
  let rec loop i value mult =
    let ch = Char.code s.[i] in
    let digit = ch land 127 in
    let value = value + (digit * mult) in
    let mult = mult * 128 in
    if ch land 128 = 0 then value, i + 1 else loop (i + 1) value mult
  in
  loop i 0 1

let decode s =
  let typ = message_type_of_byte (Char.code s.[0]) in
  let len, i = decode_length s 1 in
  Decoder.decode_packet typ (Mqtt.Read_buffer.make (String.sub s i len))
  
let test_connect_encdec () =
  let c = {
    client_id = "id";
    credentials = None;
    will = None;
    clean_session = false;
    keep_alive = 0;
  } in
  let pkt = Connect c in
  let data = Encoder.message pkt in
  Printf.printf "%S\n" data;
  let pkt' = decode data in
  (* TODO: fix pp *)
  Alcotest.(check (testable Fmt.nop ( = )) "TODO" pkt pkt')

let test_publish_encdec () = ()

let test_subscribe_encdec () = ()

(* Run the tests *)
let () = Alcotest.(
  run "Mqtt_packet" [
      ( "connect",
        [
          test_case "Encode/decode" `Quick test_connect_encdec;
      ] );
      ( "publish",
        [
          test_case "Encode/decode" `Quick test_publish_encdec;
      ] );
      ( "subscribe",
        [
          test_case "Encode/decode" `Quick test_subscribe_encdec;
      ] );
    ]
)
