module Sound exposing (..)

import Json.Encode
import Ports


play : String -> Cmd msg
play name =
    Json.Encode.object [ ( "play", Json.Encode.string name ) ]
        |> Ports.sounds


loop : String -> Cmd msg
loop sound =
    Json.Encode.object [ ( "loop", Json.Encode.string sound ) ]
        |> Ports.sounds


stop : String -> Cmd msg
stop sound =
    Json.Encode.object [ ( "stop", Json.Encode.string sound ) ]
        |> Ports.sounds


mute : Bool -> Cmd msg
mute value =
    Json.Encode.object [ ( "mute", Json.Encode.bool value ) ]
        |> Ports.sounds
