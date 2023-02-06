type t

exception Connection_error

val connect :
  ?id:string ->
  ?tls_ca:string ->
  ?credentials:Types.credentials ->
  ?will:Packet.message ->
  ?clean_session:bool ->
  ?keep_alive:int ->
  ?on_message:(topic:string -> string -> unit Lwt.t) ->
  ?on_disconnect:(t -> unit Lwt.t) ->
  ?on_error:(t -> exn -> unit Lwt.t) ->
  ?port:int ->
  string list ->
  t Lwt.t
(** Connects to the MQTT broker.

    Multiple hosts can be provided in case the broker supports failover. The
    client will attempt to connect to each one of hosts sequentially until one
    of them is successful.

    [on_error] can be provided to handle errors during client's execution. By
    default all internal exceptions will be raised with [Lwt.fail].

    {i Note:} Reconnection logic is not implemented currently.

    {[
      let broker_hosts = [ ("host-1", "host-2") ] in

      let on_message ~topic payload =
        Lwt.printlf "topic=%S payload=%S" topic payload
      in

      Mqtt_client.connect ~id:"my-client" ~port:1883 ~on_message broker_hosts
      |> Lwt_main.run
    ]} *)

val disconnect : t -> unit Lwt.t
(** Disconnects the client from the MQTT broker.

    {[
      let%lwt () = Mqtt_client.disconnect client
    ]} *)

val publish :
  ?dup:bool ->
  ?qos:Types.qos ->
  ?retain:bool ->
  topic:string ->
  string ->
  t ->
  unit Lwt.t
(** Publish a message with payload to a given topic.

    {[
      let payload = "Hello world";
      let%lwt () = Mqtt_client.publish(~topic="news", payload, client);
    ]} *)

val subscribe : (string * Types.qos) list -> t -> unit Lwt.t
(** Subscribes the client to a non-empty list of topics.

    {[
      let topics =
        [
          ("news/fashion", Mqtt_client.Atmost_once);
          ("news/science", Mqtt_client.Atleast_once);
        ]
      in
      Mqtt_client.subscribe topics client
    ]} *)
