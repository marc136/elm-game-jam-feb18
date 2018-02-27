port module Ports exposing (..)

import Json.Encode


port sounds : Json.Encode.Value -> Cmd msg
