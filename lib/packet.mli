open Types

type publish_options = {
  dup : bool;
  qos : qos;
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

val message_type_of_byte : int -> message_type

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

type connect = {
  client_id : string;
  credentials : credentials option;
  will : message option;
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
  | Suback of int * (qos, unit) result list
  | Unsubscribe of int * string list
  | Unsuback of int
  | Publish of publish
  | Puback of int
  | Pubrec of int
  | Pubrel of int
  | Pubcomp of int
  | Pingreq
  | Pingresp
  | Disconnect

module Encoder : sig

  val unsubscribe : id:int -> string list -> string

  val unsuback : int -> string

  val pingreq : unit -> string

  val pingresp : unit -> string

  val pubrec : int -> string

  val pubrel : int -> string

  val pubcomp : int -> string

  val suback : int -> (qos, unit) result list -> string

  val puback : int -> string

  val disconnect : unit -> string

  val subscribe : id:int -> (string * qos) list -> string

  val publish :
    dup:bool ->
    qos:qos ->
    retain:bool -> ?id:int -> topic:string -> string -> string

  val connect_payload :
    ?credentials:credentials ->
    ?will:message ->
    ?clean_session:bool -> ?keep_alive:int -> string -> string

  val connect :
    ?credentials:credentials ->
    ?will:message ->
    ?clean_session:bool -> ?keep_alive:int -> string -> string

  val connack : session_present:bool -> connection_status -> string

  val message : t -> string
end

module Decoder : sig

  val decode_packet : message_type -> Read_buffer.t -> t

end
