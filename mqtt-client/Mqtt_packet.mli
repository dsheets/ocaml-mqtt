
type message_type =
  | Connect_pkt
  | Connack_pkt
  | Publish_pkt
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

type cxn_data = {
  clientid : string;
  credentials : Mqtt_core.credentials option;
  will : will option;
  clean_session : bool;
  keep_alive : int;
}

type client_options = {
  ping_timeout : float;
  cxn_data : cxn_data;
}

type connection_status =
  | Accepted
  | Unacceptable_protocol_version
  | Identifier_rejected
  | Server_unavailable
  | Bad_username_or_password
  | Not_authorized

val connection_status_to_string : connection_status -> string

val connection_status_to_int : connection_status -> int

val connection_status_of_int : int -> connection_status

type t =
  | Connect of cxn_data
  | Connack of { session_present : bool;
      connection_status : connection_status;
    }
  | Subscribe of (int * (string * Mqtt_core.qos) list)
  | Suback of (int * (Mqtt_core.qos, unit) result list)
  | Unsubscribe of (int * string list)
  | Unsuback of int
  | Publish of (int option * string * string)
  | Puback of int
  | Pubrec of int
  | Pubrel of int
  | Pubcomp of int
  | Pingreq
  | Pingresp
  | Disconnect

type options = {
  dup : bool;
  qos : Mqtt_core.qos;
  retain : bool;
}

module Encoder : sig

  val unsubscribe : id:int -> string list -> string

  val unsuback : int -> string

  val pingreq : unit -> string

  val pingresp : unit -> string

  val pubrec : int -> string

  val pubrel : int -> string

  val pubcomp : int -> string

  val suback : int -> Mqtt_core.qos list -> string

  val puback : int -> string

  val disconnect : unit -> string

  val subscribe : id:int -> (string * Mqtt_core.qos) list -> string

  val publish :
    dup:bool ->
    qos:Mqtt_core.qos ->
    retain:bool -> id:int -> topic:string -> string -> string

  val connect_payload :
    ?credentials:Mqtt_core.credentials ->
    ?will:will ->
    ?clean_session:bool -> ?keep_alive:int -> string -> string

  val connect :
    ?credentials:Mqtt_core.credentials ->
    ?will:will ->
    ?clean_session:bool -> ?keep_alive:int -> string -> string

  val connect_data : cxn_data -> string

  val connack : session_present:bool -> connection_status -> string
end

module Decoder : sig

  val decode_packet : options -> message_type -> Read_buffer.t -> t

  val decode_fixed_header : int -> message_type * options
end
