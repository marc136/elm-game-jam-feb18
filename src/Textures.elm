module Textures exposing (load, textures)

import Game.Resources as Resources exposing (Resources)
import WebGL.Texture as Texture exposing (Texture, defaultOptions)


availableTextures : List String
availableTextures =
    [ textures.worker
    , textures.background
    , textures.hat
    ]


textures =
    { background = "images/background.png"
    , worker = "images/worker.png"
    , hat = "images/hat.png"
    }


load : (Resources.Msg -> msg) -> (String -> msg) -> Cmd msg
load success error =
    availableTextures
        |> List.map texture
        |> Resources.loadTexturesWithConfig
            { success = success
            , failed = error
            }


texture : String -> ( Texture.Options, String )
texture url =
    ( { defaultOptions | magnify = Texture.nearest }, url )
