module Types exposing (..)

import Array exposing (Array)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera as Camera exposing (Camera)
import Keyboard.Extra
import PageVisibility exposing (Visibility)


---- MODEL ----


type alias Model =
    { stage : Stage

    -- level specific
    , level : Int
    , dude : Dude
    , queue : List Int

    {--TODO decide if walking workers should be in one list instead-}
    , rows : Workers
    , hats : List Hat
    , spawnSpeed : Float
    , workerSpeed : Float

    -- game stats
    , stats : LevelStats
    , costs : Int

    -- general stuff
    , pressedKeys : List Keyboard.Extra.Key
    , time : Float
    , nextSpawn : Float
    , resources : Resources
    , screen : ( Int, Int )
    , camera : Camera
    , visible : Bool
    }


type Stage
    = Menu
    | BeforeLevel
    | PlayLevel
    | PauseLevel
    | GameOver


type alias Dude =
    { x : Float
    , y : Float
    , animate : Animate
    , row : Int
    , hasHat : Bool
    }


type Animate
    = Idle
    | MoveUp Float
    | MoveDown Float
    | Throw Float
    | GetHat
    | ReturnWithHat


type alias Workers =
    Array (List Worker)


type alias Worker =
    { x : Float
    , y : Float
    , lifetime : Float
    , moveSpeed : Float
    , hasHat : Bool
    }


type alias Hat =
    { row : Int
    , x : Float
    }


type alias LevelStats =
    { workers : Int
    , withoutHats : Int
    }



---- UPDATE ----


type Msg
    = Resources Resources.Msg
    | ResourcesErr String
    | LoadLevel Int
    | StartLevel
    | KeyMsg Keyboard.Extra.Msg
    | TogglePause
    | PageVisible Visibility
    | Tick Float
    | Restart
