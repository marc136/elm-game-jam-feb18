module View.WebGl exposing (render)

import Array exposing (Array)
import Common
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, button, div, h1, h2, p, text)
import Textures exposing (textures)
import Types exposing (..)


render : Model -> Html msg
render model =
    Game.render (renderConfig model) (toRenderables model)


renderConfig : { d | camera : Camera, screen : ( Int, Int ), time : Float } -> Game.RenderConfig
renderConfig { camera, time, screen } =
    { camera = camera
    , time = time
    , size = screen
    }


toRenderables : Model -> List Renderable
toRenderables ({ resources, dude, rows } as model) =
    List.concat
        [ [ background resources ]
        , List.concatMap (row resources) (Array.toList rows)
        , [ renderDude resources dude ]
        , List.map (hat resources) model.hats
        ]


background : Resources -> Renderable
background resources =
    Render.spriteZ
        { position = ( -20, -15, 0 )
        , size = ( 40, 30 )
        , texture = Resources.getTexture textures.background resources
        }


renderDude : Resources -> Dude -> Renderable
renderDude resources dude =
    workerSprite resources dude <|
        spritePosition <|
            case dude.animate of
                GetHat ->
                    WalkWithoutHat 1

                ReturnWithHat ->
                    WalkWithHat -1

                _ ->
                    StandIdle -1


row : Resources -> List Worker -> List Renderable
row resources workers =
    List.map (worker resources) workers


worker : Resources -> Worker -> Renderable
worker resources worker =
    workerSprite resources
        worker
        (spritePosition <|
            if worker.hasHat then
                WalkWithHat 1
            else
                WalkWithoutHat 1
        )


spritePosition : WorkerSprites -> SpriteSetting
spritePosition kind =
    let
        worker =
            0.1494140625
    in
    case kind of
        WalkWithHat direction ->
            SpriteSetting ( 0, 0 ) ( 1, worker ) 8 direction

        WalkWithoutHat direction ->
            SpriteSetting ( 0, worker ) ( 1, 2 * worker ) 8 direction

        StandIdle direction ->
            SpriteSetting ( 0, 2 * worker ) ( 0.5, 3 * worker ) 4 direction


type alias SpriteSetting =
    { bottomLeft : ( Float, Float )
    , topRight : ( Float, Float )
    , frames : Int
    , direction : Float
    }


type WorkerSprites
    = WalkWithHat Float
    | WalkWithoutHat Float
    | StandIdle Float


workerSprite : Resources -> { a | x : Float, y : Float, hasHat : Bool } -> SpriteSetting -> Renderable
workerSprite resources { x, y, hasHat } setting =
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( setting.direction * 6, 8 )
        , texture = Resources.getTexture textures.worker resources
        , bottomLeft = setting.bottomLeft
        , topRight = setting.topRight
        , duration =
            if hasHat then
                1
            else
                1.8
        , numberOfFrames = setting.frames
        , rotation = 0
        , pivot = ( 0.5, 1 )
        }


hat : Resources -> Hat -> Renderable
hat resources { x, row } =
    Render.animatedSpriteWithOptions
        { position = ( x, Common.rowToY row - 0, 0 )

        -- if a size value is < 0 the image is flipped
        , size = ( 3.5, 4 )
        , texture = Resources.getTexture textures.worker resources

        -- bottomLeft and topRight allow to select a subsection of an image ( x, y )
        , bottomLeft = ( 0, 0.5029296875 )
        , topRight = ( 0.380859375, 0.5029296875 + 0.068359375 )
        , duration = 1.2
        , numberOfFrames = 5
        , rotation = 0

        -- pivot point for rotation
        , pivot = ( 0.5, 1 )
        }
