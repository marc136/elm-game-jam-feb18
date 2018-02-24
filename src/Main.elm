module Main exposing (..)

import AnimationFrame
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, div, h1, img, text)
import Keyboard.Extra
import WebGL.Texture as Texture exposing (Texture)


--import Math.Vector2 exposing (Vec2, vec2)
--import Math.Vector3 exposing (Vec3, vec3)
--import Task
--import Window
---- MODEL ----


type alias Model =
    { dude : Dude
    , resources : Resources
    , pressedKeys : List Keyboard.Extra.Key
    , time : Float
    , screen : ( Int, Int )
    , camera : Camera
    , rows : List (List Worker)
    , angryWorkers : List Worker
    }


type Animate
    = Idle
    | MoveUp Float
    | MoveDown Float


moveSpeed : Float
moveSpeed =
    5


type alias Dude =
    { x : Float
    , y : Float
    , animate : Animate
    , row : Int
    }


initDude : Dude
initDude =
    { x = 12
    , y = rowToY 0
    , animate = Idle
    , row = 0
    }


rowToY : Int -> Float
rowToY row =
    6 - (toFloat row * 5)


type alias Worker =
    { x : Float
    , y : Float
    , lifetime : Float
    , moveSpeed : Float
    }


newWorker : Int -> Worker
newWorker row =
    { x = -23
    , y = rowToY row
    , lifetime = 0
    , moveSpeed = 1.5
    }



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( { dude = initDude
      , rows = [ [ newWorker 1 ] ]
      , angryWorkers = []
      , pressedKeys = []
      , time = 0
      , screen = ( 800, 600 )

      -- 4/3 screen size -> 40 x 30 game units
      -- check with Camera.getViewSize model.screen model.camera
      , camera = Camera.fixedArea (40 * 30) ( 0, 0 )
      , resources = Resources.init
      }
    , Cmd.batch
        [ loadTextures
            [ textures.dude
            , textures.worker
            , textures.background
            ]
        ]
    )


loadTextures : List String -> Cmd Msg
loadTextures paths =
    List.map texture paths
        |> Resources.loadTexturesWithConfig
            { success = Resources
            , failed = ResourcesErr
            }


textures =
    { dude = "images/dude.png"
    , background = "images/background.png"
    , worker = "images/worker.png"
    }


texture : String -> ( Texture.Options, String )
texture url =
    let
        default =
            Texture.defaultOptions
    in
    ( { default | magnify = Texture.nearest }, url )



---- UPDATE ----


type Msg
    = Tick Float
    | Resources Resources.Msg
    | ResourcesErr String
    | KeyMsg Keyboard.Extra.Msg


moveDude : Float -> Dude -> Dude
moveDude dt dude =
    case dude.animate of
        MoveUp destination ->
            let
                y =
                    dude.y + (moveSpeed * dt)
            in
            if y >= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        MoveDown destination ->
            let
                y =
                    dude.y - (moveSpeed * dt)
            in
            if y <= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        _ ->
            dude


handleUserInput : { a | y : Int } -> Dude -> Dude
handleUserInput { y } ({ animate, row } as dude) =
    if animate == Idle then
        if y == 1 && row > 0 then
            { dude | animate = MoveUp <| rowToY (row - 1), row = row - 1 }
        else if y == -1 && row < 4 then
            { dude | animate = MoveDown <| rowToY (row + 1), row = row + 1 }
        else
            dude
    else
        dude


moveWorker : Float -> Worker -> Worker
moveWorker delta worker =
    { worker
        | x =
            if worker.x < 15 then
                worker.x + (worker.moveSpeed * delta)
            else
                worker.x
        , lifetime = worker.lifetime + delta
    }


updateRowWithWorkers : List Worker -> ( List Worker, List Worker )
updateRowWithWorkers =
    List.partition workerKeepsWalking


workerKeepsWalking : Worker -> Bool
workerKeepsWalking worker =
    worker.x < 15


updateRows : Float -> Model -> ( Model, Cmd Msg )
updateRows delta model =
    let
        ( rows, stoppedWorkers ) =
            model.rows
                |> List.map (updateRow delta)
                |> List.unzip
                |> Tuple.mapSecond List.concat
    in
    ( { model
        | rows = rows
        , angryWorkers = List.append model.angryWorkers stoppedWorkers
      }
    , Cmd.none
    )


concatLists : ( List (List a), List (List b) ) -> ( List a, List b )
concatLists ( list1, list2 ) =
    ( List.concat list1, List.concat list2 )


updateRow : Float -> List Worker -> ( List Worker, List Worker )
updateRow delta workers =
    List.map (moveWorker delta) workers
        |> List.partition workerKeepsWalking


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            { model
                | time = model.time + delta
                , dude =
                    model.dude
                        |> moveDude delta
                        |> handleUserInput (Keyboard.Extra.arrows model.pressedKeys)
            }
                |> updateRows delta

        Resources msg ->
            ( { model | resources = Resources.update msg model.resources }
            , Cmd.none
            )

        ResourcesErr url ->
            let
                _ =
                    Debug.log "Could not load texture " url
            in
            ( model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    Game.renderWithOptions []
        (renderConfig model)
        (toRenderables model)


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
        , List.concatMap (row resources) rows
        , [ renderDude resources dude ]
        , List.map (worker resources) model.angryWorkers
        ]


background : Resources -> Renderable
background resources =
    Render.spriteZ
        { position = ( -20, -15, 0 )
        , size = ( 40, 30 )
        , texture = Resources.getTexture textures.background resources
        }


row : Resources -> List Worker -> List Renderable
row resources workers =
    List.map (worker resources) workers


worker : Resources -> Worker -> Renderable
worker resources { x, y, lifetime } =
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( 6, 8 )
        , texture = Resources.getTexture textures.worker resources
        , bottomLeft = ( 0, 0 )
        , topRight = ( 0.83984375, 1 )
        , duration = 1.2
        , numberOfFrames = 4
        , rotation = 0

        -- pivot point for rotation
        , pivot = ( 0.5, 1 )
        }


renderDude : Resources -> Dude -> Renderable
renderDude resources { x, y } =
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )

        -- if a size value is < 0 the image is flipped
        , size = ( -6, 8 )
        , texture = Resources.getTexture textures.dude resources

        -- bottomLeft and topRight allow to select a subsection of an image
        , bottomLeft = ( 0, 0 )
        , topRight = ( 0.83984375, 1 )
        , duration = 1.2
        , numberOfFrames = 4
        , rotation = 0

        -- pivot point for rotation
        , pivot = ( 0.5, 1 )
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.Extra.subscriptions

        --, Window.resizes ScreenSize
        , AnimationFrame.diffs (Tick << (\d -> d / 1000))
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
