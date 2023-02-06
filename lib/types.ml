type credentials = Credentials of string * string | Username of string

type qos = Atmost_once | Atleast_once | Exactly_once

type message = {
  topic : string;
  payload : string;
  qos : qos;
  retain : bool;
}
