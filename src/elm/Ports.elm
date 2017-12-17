port module Ports exposing (..)

import Types exposing (EncodedIncomingMessage, EncodedOutgoingMessage)

-- take an incoming message
port incomingMessage : (Maybe EncodedIncomingMessage -> msg) -> Sub msg

-- send an outgoing element
port outgoingMessage : Maybe EncodedOutgoingMessage -> Cmd msg
