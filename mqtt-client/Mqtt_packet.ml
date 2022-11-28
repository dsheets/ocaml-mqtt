module BE = EndianBytes.BigEndian
open Mqtt_core

let int16be n =
  let s = Bytes.create 2 in
  BE.set_int16 s 0 n;
  s

let int8be n =
  let s = Bytes.create 1 in
  BE.set_int8 s 0 n;
  s

type publish_options = {
  dup : bool;
  qos : Mqtt_core.qos;
  retain : bool;
}

type message_type =
  | Connect_pkt
  | Connack_pkt
  | Publish_pkt of publish_options
  | Puback_pkt
  | Pubrec_pkt
  | Pubrel_pkt
  | Pubcomp_pkt
  | Subscribe_pkt
  | Suback_pkt
  | Unsubscribe_pkt
  | Unsuback_pkt
  | Pingreq_pkt
  | Pingresp_pkt
  | Disconnect_pkt

type will = {
  topic : string;
  message : string;
  qos : Mqtt_core.qos;
  retain : bool;
}

type connection_status =
  | Accepted
  | Unacceptable_protocol_version
  | Identifier_rejected
  | Server_unavailable
  | Bad_username_or_password
  | Not_authorized

let connection_status_to_string = function
  | Accepted -> "Accepted"
  | Unacceptable_protocol_version -> "Unacceptable_protocol_version"
  | Identifier_rejected -> "Identifier_rejected"
  | Server_unavailable -> "Server_unavailable"
  | Bad_username_or_password -> "Bad_username_or_password"
  | Not_authorized -> "Not_authorized"

let connection_status_to_int = function
  | Accepted -> 0
  | Unacceptable_protocol_version -> 1
  | Identifier_rejected -> 2
  | Server_unavailable -> 3
  | Bad_username_or_password -> 4
  | Not_authorized -> 5

let connection_status_of_int = function
  | 0 -> Accepted
  | 1 -> Unacceptable_protocol_version
  | 2 -> Identifier_rejected
  | 3 -> Server_unavailable
  | 4 -> Bad_username_or_password
  | 5 -> Not_authorized
  | _ -> raise (Invalid_argument "Invalid connection status code")

type connect = {
  client_id : string;
  credentials : credentials option;
  will : will option;
  clean_session : bool;
  keep_alive : int;
}

type connack = {
  session_present : bool;
  connection_status : connection_status;
}

type subscribe = {
  message_id : int;
  topics : (string * qos) list;
}

type publish = {
  options : publish_options;
  message_id : int option;
  topic : string;
  payload : string;
}

type t =
  | Connect of connect
  | Connack of connack
  | Subscribe of subscribe
  | Suback of (int * (qos, unit) result list)
  | Unsubscribe of (int * string list)
  | Unsuback of int
  | Publish of publish
  | Puback of int
  | Pubrec of int
  | Pubrel of int
  | Pubcomp of int
  | Pingreq
  | Pingresp
  | Disconnect

let bits_of_qos = function
  | Atmost_once -> 0
  | Atleast_once -> 1
  | Exactly_once -> 2

let qos_of_bits = function
  | 0 -> Atmost_once
  | 1 -> Atleast_once
  | 2 -> Exactly_once
  | b -> raise (Invalid_argument ("invalid qos number: " ^ string_of_int b))

let suback_qos_of_bits = function 0x80 -> Error () | b -> Ok (qos_of_bits b)
let bit_of_bool = function true -> 1 | false -> 0

let bool_of_bit = function
  | 1 -> true
  | 0 -> false
  | n ->
    raise
      (Invalid_argument ("expected zero or one, but got " ^ string_of_int n))

let byte_of_message_type = function
  | Connect_pkt      ->  1 lsl 4
  | Connack_pkt      ->  2 lsl 4
  | Puback_pkt       ->  4 lsl 4
  | Pubrec_pkt       ->  5 lsl 4
  | Pubrel_pkt       ->  6 lsl 4 + 2
  | Pubcomp_pkt      ->  7 lsl 4
  | Subscribe_pkt    ->  8 lsl 4 + 2
  | Suback_pkt       ->  9 lsl 4
  | Unsubscribe_pkt  -> 10 lsl 4 + 2
  | Unsuback_pkt     -> 11 lsl 4
  | Pingreq_pkt      -> 12 lsl 4
  | Pingresp_pkt     -> 13 lsl 4
  | Disconnect_pkt   -> 14 lsl 4
  | Publish_pkt opts ->
    (3 lsl 4) +
      (if opts.dup then 1 lsl 3 else 0) +
      (bits_of_qos opts.qos lsl 1) +
      bit_of_bool opts.retain

let message_type_of_byte byte = match byte with
  | 16  -> Connect_pkt     (* 1 *)
  | 32  -> Connack_pkt     (* 2 *)
  | 64  -> Puback_pkt      (* 4 *)
  | 80  -> Pubrec_pkt      (* 5 *)
  | 98  -> Pubrel_pkt      (* 6 + reserved *)
  | 112 -> Pubcomp_pkt     (* 7 *)
  | 130 -> Subscribe_pkt   (* 8 + reserved *)
  | 144 -> Suback_pkt      (* 9 *)
  | 162 -> Unsubscribe_pkt (* 10 + reserved *)
  | 176 -> Unsuback_pkt    (* 11 *)
  | 192 -> Pingreq_pkt     (* 12 *)
  | 208 -> Pingresp_pkt    (* 13 *)
  | 224 -> Disconnect_pkt  (* 14 *)
  | _   ->
    if byte lsr 4 = 3 (* publish *)
    then
      let dup = byte land 0x08 <> 0 in
      let qos = qos_of_bits ((byte land 0x06) lsr 1) in
      let retain = byte land 0x01 <> 0 in
      Publish_pkt { dup; qos; retain }
    else raise (Invalid_argument ("invalid bits " ^ string_of_int byte ^ " for message_type"))

let trunc str =
  (* truncate leading zeroes *)
  let len = String.length str in
  let rec loop count =
    if count >= len || str.[count] <> '\000' then count else loop (count + 1)
  in
  let leading = loop 0 in
  if leading = len then "\000" else String.sub str leading (len - leading)

let addlen s =
  let len = String.length s in
  if len > 0xFFFF then raise (Invalid_argument "string too long");
  Bytes.to_string (int16be len) ^ s

let opt_with s n = function Some a -> s a | None -> n

module Encoder = struct
  let encode_length len =
    let rec loop ll digits =
      if ll <= 0 then digits
      else
        let incr = Int32.logor (Int32.of_int 0x80) in
        let shft = Int32.logor (Int32.shift_left digits 8) in
        let getdig x dig = if x > 0 then incr dig else dig in
        let quotient = ll / 128 in
        let digit = getdig quotient (Int32.of_int (ll mod 128)) in
        let digits = shft digit in
        loop quotient digits
    in
    loop len 0l

  let fixed_header typ body_len =
    let len = Bytes.create 4 in
    BE.set_int32 len 0 (encode_length body_len);
    let len = trunc (Bytes.to_string len) in
    String.make 1 (char_of_int (byte_of_message_type typ)) ^ len

  let unsubscribe ~id topics =
    let accum acc i = acc + 2 + String.length i in
    let tl = List.fold_left accum 2 topics in
    (* +2 for msgid *)
    let buf = Buffer.create (tl + 5) in
    (* ~5 for fixed header *)
    let addtopic t = addlen t |> Buffer.add_string buf in
    let msgid = int16be id |> Bytes.to_string in
    let hdr = fixed_header Unsubscribe_pkt tl in
    Buffer.add_string buf hdr;
    Buffer.add_string buf msgid;
    List.iter addtopic topics;
    Buffer.contents buf

  let unsuback id =
    let msgid = int16be id |> Bytes.to_string in
    let hdr = fixed_header Unsuback_pkt 2 in
    hdr ^ msgid

  let simple_pkt typ = fixed_header typ 0
  let pingreq () = simple_pkt Pingreq_pkt
  let pingresp () = simple_pkt Pingresp_pkt

  let pubpkt typ id =
    let hdr = fixed_header typ 2 in
    let msgid = int16be id |> Bytes.to_string in
    let buf = Buffer.create 4 in
    Buffer.add_string buf hdr;
    Buffer.add_string buf msgid;
    Buffer.contents buf

  let pubrec = pubpkt Pubrec_pkt
  let pubrel = pubpkt Pubrel_pkt
  let pubcomp = pubpkt Pubcomp_pkt

  let suback id qoses =
    let paylen = List.length qoses + 2 in
    let buf = Buffer.create (paylen + 5) in
    let msgid = int16be id |> Bytes.to_string in
    let q2i q = bits_of_qos q |> int8be |> Bytes.to_string in
    let blit q = Buffer.add_string buf (q2i q) in
    let hdr = fixed_header Suback_pkt paylen in
    Buffer.add_string buf hdr;
    Buffer.add_string buf msgid;
    List.iter blit qoses;
    Buffer.contents buf

  let puback = pubpkt Puback_pkt
  let disconnect () = simple_pkt Disconnect_pkt

  let subscribe ~id topics =
    let accum acc (i, _) = acc + 3 + String.length i in
    let tl = List.fold_left accum 0 topics in
    let tl = tl + 2 in
    (* add msgid to total len *)
    let buf = Buffer.create (tl + 5) in
    (* ~5 for fixed header *)
    let addtopic (t, q) =
      Buffer.add_string buf (addlen t);
      Buffer.add_string buf (Bytes.to_string @@ int8be (bits_of_qos q))
    in
    let msgid = int16be id |> Bytes.to_string in
    let hdr = fixed_header Subscribe_pkt tl in
    Buffer.add_string buf hdr;
    Buffer.add_string buf msgid;
    List.iter addtopic topics;
    Buffer.contents buf

  let publish ~dup ~qos ~retain ~id ~topic payload =
    let id_data =
      if qos = Atleast_once || qos = Exactly_once then
        Bytes.to_string (int16be id)
      else ""
    in
    let dup = if qos = Atmost_once then false else dup in
    let topic = addlen topic in
    let sl = String.length in
    let tl = sl topic + sl payload + sl id_data in
    let buf = Buffer.create (tl + 5) in
    let hdr = fixed_header (Publish_pkt { dup; qos; retain }) tl in
    Buffer.add_string buf hdr;
    Buffer.add_string buf topic;
    Buffer.add_string buf id_data;
    Buffer.add_string buf payload;
    Buffer.contents buf

  let connect_payload ?credentials ?will ?(clean_session = false) ?(keep_alive = 10) id =
    let name = addlen "MQTT" in
    let version = "\004" in
    let clean_session_bit = 0x02 in
    let will_retain_bit = 0x20 in
    let qos_bits qos = bits_of_qos qos lsl 3 in
    if keep_alive > 0xFFFF then raise (Invalid_argument "keep_alive too large");
    let addbit switch bit (flags, hdr) =
      (if switch then flags lor bit else flags), hdr
    in
    let addhdr2 flag a b (flags, hdr) =
      (flags lor flag, hdr ^ addlen a ^ addlen b)
    in
    let adduserpass term (flags, hdr) =
      match term with
      | None -> (flags, hdr)
      | Some (Username s) -> (flags lor 0x80, hdr ^ addlen s)
      | Some (Credentials (u, p)) -> addhdr2 0xC0 u p (flags, hdr)
    in
    let flags, pay =
      (0, addlen id)
      |> addbit clean_session clean_session_bit
      |> opt_with (fun {topic; message; qos; retain} (flags, hdr) ->
             (flags lor qos_bits qos, hdr)
             |> addbit retain will_retain_bit
             |> addhdr2 0x04 topic message
           ) (fun x -> x) will
      |> adduserpass credentials
    in
    let tbuf = int16be keep_alive in
    let fbuf = Bytes.create 1 in
    BE.set_int8 fbuf 0 flags;
    let accum acc a = acc + String.length a in
    let fields =
      [ name; version; Bytes.to_string fbuf; Bytes.to_string tbuf; pay ]
    in
    let lens = List.fold_left accum 0 fields in
    let buf = Buffer.create lens in
    List.iter (Buffer.add_string buf) fields;
    Buffer.contents buf

  let connect ?credentials ?will ?clean_session ?keep_alive id =
    let cxn_pay = connect_payload ?credentials ?will ?clean_session ?keep_alive id in
    let hdr = fixed_header Connect_pkt (String.length cxn_pay) in
    hdr ^ cxn_pay

  let connect_data d =
    let client_id = d.client_id in
    let credentials = d.credentials in
    let will = d.will in
    let clean_session = d.clean_session in
    let keep_alive = d.keep_alive in
    connect_payload ?credentials ?will ~clean_session ~keep_alive client_id

  let connack ~session_present status =
    let fixed_header = fixed_header Connack_pkt 2 in
    let flags = Bytes.to_string (int8be (bit_of_bool session_present)) in
    let connection_status =
      Bytes.to_string (int8be (connection_status_to_int status))
    in
    let variable_header = flags ^ connection_status in
    fixed_header ^ variable_header
end

module Decoder = struct
  let decode_connect rb =
    let lead = Read_buffer.read rb 9 in
    if "\000\004MQTT\004" <> lead then
      raise (Invalid_argument "invalid MQTT or version");
    let hdr = Read_buffer.read_uint8 rb in
    let keep_alive = Read_buffer.read_uint16 rb in
    let has_username = 0 <> hdr land 0x80 in
    let has_password = 0 <> hdr land 0xC0 in
    let will_flag = bool_of_bit ((hdr land 0x04) lsr 2) in
    let clean_session = bool_of_bit ((hdr land 0x02) lsr 1) in
    let rs = Read_buffer.read_string in
    let client_id = rs rb in
    let will =
      if will_flag then
        let qos = qos_of_bits ((hdr land 0x18) lsr 3) in
        let retain = 0 <> hdr land 0x20 in
        let topic = rs rb in
        let message = rs rb in
        Some {topic; message; qos; retain}
      else None
    in
    let credentials =
      if has_password then
        let u = rs rb in
        let p = rs rb in
        Some (Credentials (u, p))
      else if has_username then Some (Username (rs rb))
      else None
    in
    Connect { client_id; credentials; will; clean_session; keep_alive }

  let decode_connack rb =
    let flags = Read_buffer.read_uint8 rb in
    let session_present = bool_of_bit flags in
    let connection_status =
      connection_status_of_int (Read_buffer.read_uint8 rb)
    in
    Connack { session_present; connection_status }

  let decode_publish (options : publish_options) rb =
    let topic = Read_buffer.read_string rb in
    let message_id =
      if options.qos = Atleast_once || options.qos = Exactly_once then
        Some (Read_buffer.read_uint16 rb)
      else None
    in
    let payload = Read_buffer.len rb |> Read_buffer.read rb in
    Publish { options; message_id; topic; payload }

  let decode_puback rb = Puback (Read_buffer.read_uint16 rb)
  let decode_pubrec rb = Pubrec (Read_buffer.read_uint16 rb)
  let decode_pubrel rb = Pubrel (Read_buffer.read_uint16 rb)
  let decode_pubcomp rb = Pubcomp (Read_buffer.read_uint16 rb)

  let decode_subscribe rb =
    let message_id = Read_buffer.read_uint16 rb in
    let get_topic rb =
      let topic = Read_buffer.read_string rb in
      let qos = Read_buffer.read_uint8 rb |> qos_of_bits in
      (topic, qos)
    in
    let topics = Read_buffer.read_all rb get_topic in
    Subscribe { message_id; topics }

  let decode_suback rb =
    let id = Read_buffer.read_uint16 rb in
    let get_qos rb = Read_buffer.read_uint8 rb |> suback_qos_of_bits in
    let qoses = Read_buffer.read_all rb get_qos in
    Suback (id, List.rev qoses)

  let decode_unsub rb =
    let id = Read_buffer.read_uint16 rb in
    let topics = Read_buffer.read_all rb Read_buffer.read_string in
    Unsubscribe (id, topics)

  let decode_unsuback rb = Unsuback (Read_buffer.read_uint16 rb)
  let decode_pingreq _rb = Pingreq
  let decode_pingresp _rb = Pingresp
  let decode_disconnect _rb = Disconnect

  let decode_packet = function
    | Connect_pkt -> decode_connect
    | Connack_pkt -> decode_connack
    | Publish_pkt opts -> decode_publish opts
    | Puback_pkt -> decode_puback
    | Pubrec_pkt -> decode_pubrec
    | Pubrel_pkt -> decode_pubrel
    | Pubcomp_pkt -> decode_pubcomp
    | Subscribe_pkt -> decode_subscribe
    | Suback_pkt -> decode_suback
    | Unsubscribe_pkt -> decode_unsub
    | Unsuback_pkt -> decode_unsuback
    | Pingreq_pkt -> decode_pingreq
    | Pingresp_pkt -> decode_pingresp
    | Disconnect_pkt -> decode_disconnect
end
