module Main exposing (..)

import AnimationFrame
import Array exposing (Array)
import Array.Extra
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Helpers
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Keyboard.Extra
import PageVisibility exposing (Visibility)
import Ports
import Textures exposing (textures)


---- MODEL ----


type alias Model =
    { stage : Stage
    , costs : Int
    , dude : Dude
    , resources : Resources
    , pressedKeys : List Keyboard.Extra.Key
    , time : Float
    , screen : ( Int, Int )
    , camera : Camera

    -- TODO decide if walking workers should be in one list instead
    , rows : Array (List Worker)
    , angryWorkers : List Worker
    , happyWorkers : List Worker
    , hats : List Hat
    , visible : Bool
    , paused : Bool
    }


type Stage
    = Menu
    | Level (List Int) Float
    | GameOver


type alias Hat =
    { row : Int
    , x : Float
    }


constants =
    { moveSpeed = 5
    , throwSpeed = 1.0
    , hatSpeed = 8
    , dudeX = 18
    , outsideViewport = 23
    }


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


initDude : Dude
initDude =
    { x = constants.dudeX
    , y = rowToY 0
    , animate = Idle
    , row = 0
    , hasHat = True
    }


rowToY : Int -> Float
rowToY row =
    13 - (toFloat row * 5)


type alias Worker =
    { x : Float
    , y : Float
    , lifetime : Float
    , moveSpeed : Float
    , hasHat : Bool
    }


newWorker : Int -> Worker
newWorker row =
    { x = -1 * constants.outsideViewport
    , y = rowToY row
    , lifetime = 0
    , moveSpeed = 2
    , hasHat = False
    }



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( { stage = Menu
      , costs = 0
      , dude = initDude
      , rows = Array.empty
      , angryWorkers = []
      , happyWorkers = []
      , pressedKeys = []
      , hats = []
      , time = 0
      , screen = ( 800, 600 )

      -- 4/3 screen size -> 40 x 30 game units
      -- check with Camera.getViewSize model.screen model.camera
      , camera = Camera.fixedArea (40 * 30) ( 0, 0 )
      , resources = Resources.init
      , visible = True
      , paused = False
      }
        |> loadLevel 0
    , Cmd.batch
        [ Textures.load Resources ResourcesErr
        ]
    )


loadLevel : Int -> Model -> Model
loadLevel index model =
    let
        model_ =
            resetLevel model

        length list =
            List.maximum list |> Maybe.withDefault 0

        level list spawnSpeed =
            case list of
                head :: tail ->
                    { model_
                        | stage = Level tail spawnSpeed
                        , rows =
                            Array.initialize
                                (length list + 1)
                                (\index ->
                                    if index == head then
                                        [ newWorker head ]
                                    else
                                        []
                                )
                    }

                _ ->
                    model
    in
    case index of
        0 ->
            level [ 0, 3, 1, 3 ] 3

        _ ->
            { model_ | stage = GameOver }


resetLevel : Model -> Model
resetLevel model =
    { model
        | stage = Menu
        , dude = initDude
        , rows = Array.empty
        , angryWorkers = []
        , happyWorkers = []
    }



---- UPDATE ----


type Msg
    = Tick Float
    | Resources Resources.Msg
    | ResourcesErr String
    | KeyMsg Keyboard.Extra.Msg
    | PageVisible Visibility
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            togglePause model

        Tick delta ->
            gameTick (Keyboard.Extra.arrows model.pressedKeys) delta model

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
            let
                keys =
                    Keyboard.Extra.update keyMsg model.pressedKeys
            in
            if List.member Keyboard.Extra.Escape keys then
                togglePause model
            else
                ( { model | pressedKeys = keys }, Cmd.none )

        PageVisible visibility ->
            ( { model | visible = visibility == PageVisibility.Visible }
            , Cmd.none
            )


togglePause : Model -> ( Model, Cmd msg )
togglePause model =
    ( { model | paused = not model.paused }, Cmd.none )


gameTick : Keyboard.Extra.Arrows -> Float -> Model -> ( Model, Cmd Msg )
gameTick arrows delta model =
    { model | time = model.time + delta }
        |> updateDude arrows delta
        |> maybeThrow arrows
        |> updateRows delta
        |> updateHats delta
        |> spawnNewWorker


updateDude : Keyboard.Extra.Arrows -> Float -> Model -> ( Model, Cmd Msg )
updateDude arrows delta model =
    let
        ( dude, cmd ) =
            model.dude
                |> moveDude delta
                |> moveDudeWithKeys arrows
    in
    ( { model | dude = dude }, cmd )


moveDude : Float -> Dude -> Dude
moveDude delta dude =
    case dude.animate of
        MoveUp destination ->
            let
                y =
                    dude.y + (constants.moveSpeed * delta)
            in
            if y >= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        MoveDown destination ->
            let
                y =
                    dude.y - (constants.moveSpeed * delta)
            in
            if y <= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        Throw time ->
            if time > 0 then
                { dude | animate = Throw (time - delta) }
            else
                { dude | animate = GetHat }

        GetHat ->
            if dude.x < constants.outsideViewport then
                { dude
                    | x = dude.x + (constants.moveSpeed * delta)
                    , animate = GetHat
                }
            else
                -- emit ka-ching sound?
                { dude
                    | animate = ReturnWithHat
                    , hasHat = True
                }

        ReturnWithHat ->
            { dude
                | x = dude.x - (constants.moveSpeed * delta)
                , animate =
                    if dude.x > constants.dudeX then
                        ReturnWithHat
                    else
                        Idle
            }

        Idle ->
            dude


moveDudeWithKeys : { a | y : Int } -> Dude -> ( Dude, Cmd Msg )
moveDudeWithKeys { y } ({ animate, row } as dude) =
    if animate == Idle then
        if y == 1 && row > 0 then
            ( { dude | animate = MoveUp <| rowToY (row - 1), row = row - 1 }
            , Ports.sound "walk"
            )
        else if y == -1 && row < 4 then
            ( { dude | animate = MoveDown <| rowToY (row + 1), row = row + 1 }
            , Ports.sound "walk"
            )
        else
            ( dude, Cmd.none )
    else
        ( dude, Cmd.none )


maybeThrow : Keyboard.Extra.Arrows -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeThrow { x } ( { dude } as model, cmd ) =
    if model.dude.animate == Idle && x == -1 then
        ( { model
            | dude = { dude | animate = Throw 0.3, hasHat = False }
            , hats = Hat model.dude.row model.dude.x :: model.hats
          }
        , Cmd.batch [ Ports.sound "throw", cmd ]
        )
    else
        ( model, cmd )


updateRows : Float -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateRows delta ( model, cmd ) =
    let
        ( rows, stoppedWorkers ) =
            Array.toList model.rows
                |> List.map (updateRow delta)
                |> List.unzip
                |> Tuple.mapSecond List.concat

        ( happyWorkers, angryWorkers ) =
            stoppedWorkers
                |> List.partition isHappyWorker
    in
    ( { model
        | rows = Array.fromList rows
        , angryWorkers = List.append stoppedWorkers model.angryWorkers
        , happyWorkers = List.append happyWorkers model.happyWorkers
      }
    , Cmd.batch
        [ if happyWorkers == [] then
            Cmd.none
          else
            Ports.sound "happyworker"
        , if angryWorkers == [] then
            Cmd.none
          else
            Ports.sound "angryworker"
        , cmd
        ]
    )


updateRow : Float -> List Worker -> ( List Worker, List Worker )
updateRow delta workers =
    List.map (moveWorker delta) workers
        |> List.partition workerKeepsWalking


moveWorker : Float -> Worker -> Worker
moveWorker delta worker =
    { worker
        | x = worker.x + (worker.moveSpeed * delta)
        , lifetime = worker.lifetime + delta
    }


workerKeepsWalking : Worker -> Bool
workerKeepsWalking worker =
    worker.x < constants.outsideViewport


isHappyWorker : Worker -> Bool
isHappyWorker worker =
    worker.hasHat


updateHats : Float -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateHats delta ( model, cmd ) =
    let
        hats =
            model.hats
                -- move hats
                |> List.map (moveHat delta)
    in
    hats
        -- check which hats hit workers
        |> List.indexedMap (hatHits model.rows)
        |> List.filterMap identity
        -- update workers and list of hats
        |> List.foldl addHatToWorker ( { model | hats = hats }, [ cmd ] )
        -- batch all commands
        |> Tuple.mapSecond Cmd.batch


moveHat : Float -> Hat -> Hat
moveHat delta hat =
    { hat | x = hat.x - delta * constants.hatSpeed }


hatHits : Array (List Worker) -> Int -> Hat -> Maybe ( Int, Int, Int )
hatHits rows hatIndex hat =
    case Array.get hat.row rows of
        Nothing ->
            Nothing

        Just workers ->
            -- updateFirst (\worker -> worker.x <= hat.x) (\worker.hasHat = True) workers
            case
                Helpers.findFirst
                    (\worker ->
                        worker.x >= hat.x && worker.x < (constants.dudeX + 1)
                    )
                    workers
            of
                Nothing ->
                    Nothing

                Just workerIndex ->
                    Just ( hatIndex, hat.row, workerIndex )


addHatToWorker : ( Int, Int, Int ) -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
addHatToWorker ( hatIndex, row, workerIndex ) ( model, cmds ) =
    ( { model
        | hats = Helpers.dropFromList hatIndex model.hats
        , rows =
            Array.Extra.update row
                (\workers ->
                    List.indexedMap
                        (\index worker ->
                            if index == workerIndex then
                                if worker.hasHat then
                                    { worker
                                        | hasHat = False
                                        , moveSpeed = worker.moveSpeed / 3
                                    }
                                else
                                    { worker
                                        | hasHat = True
                                        , moveSpeed = worker.moveSpeed * 3
                                    }
                            else
                                worker
                        )
                        workers
                )
                model.rows
        , costs = model.costs + 10
      }
    , Ports.sound "get-hat" :: cmds
    )


spawnNewWorker : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
spawnNewWorker ( model, cmd ) =
    case model.stage of
        Level (row :: list) spawnSpeed ->
            if model.time >= spawnSpeed then
                ( { model
                    | time = 0 -- time = model.time - spawnSpeed
                    , stage = Level list spawnSpeed
                    , rows =
                        Array.Extra.update row (spawnWorker row) model.rows
                  }
                , Cmd.batch [ cmd, Ports.sound "spawn" ]
                )
            else
                ( model, cmd )

        _ ->
            ( model, cmd )


spawnWorker : Int -> List Worker -> List Worker
spawnWorker row workers =
    List.append workers [ newWorker row ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Game.render
            (renderConfig model)
            (toRenderables model)
        , if model.paused then
            div [ class "overlay" ]
                [ div [ class "modal" ]
                    [ h1 [] [ text "Game is paused" ]
                    , button
                        [ onClick TogglePause ]
                        [ text "continue" ]
                    ]
                ]
          else
            text ""
        ]


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
        , List.map (worker resources) model.angryWorkers
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
    workerSprite resources dude (direction dude)


direction : Dude -> Float
direction { animate } =
    if animate == GetHat then
        1
    else
        -1


row : Resources -> List Worker -> List Renderable
row resources workers =
    List.map (worker resources) workers


worker : Resources -> Worker -> Renderable
worker resources worker =
    workerSprite resources worker 1


workerSprite : Resources -> { a | x : Float, y : Float, hasHat : Bool } -> Float -> Renderable
workerSprite resources { x, y, hasHat } direction =
    let
        row =
            if hasHat then
                0
            else
                1

        height =
            0.1494140625
    in
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( direction * 6, 8 )
        , texture = Resources.getTexture textures.worker resources
        , bottomLeft = ( 0, row * height )
        , topRight = ( 1, (row + 1) * height )
        , duration = 1.2
        , numberOfFrames = 4
        , rotation = 0
        , pivot = ( 0.5, 1 )
        }


hat : Resources -> Hat -> Renderable
hat resources { x, row } =
    Render.animatedSpriteWithOptions
        { position = ( x, rowToY row, 0 )

        -- if a size value is < 0 the image is flipped
        , size = ( 3, 4 )
        , texture = Resources.getTexture textures.hat resources

        -- bottomLeft and topRight allow to select a subsection of an image ( x, y )
        , bottomLeft = ( 0, 0 )
        , topRight = ( 1, 1 )
        , duration = 1.2
        , numberOfFrames = 4
        , rotation = 0

        -- pivot point for rotation
        , pivot = ( 0.5, 1 )
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.append
            (if model.visible && not model.paused then
                [ AnimationFrame.diffs (Tick << (\d -> d / 1000))

                --, Window.resizes ScreenSize
                ]
             else
                []
            )
            [ Sub.map KeyMsg Keyboard.Extra.subscriptions
            , PageVisibility.visibilityChanges PageVisible
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
