module Main exposing (..)

import AnimationFrame
import Array
import Common
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera as Camera exposing (Camera)
import Helpers
import Html exposing (Html, button, div, h1, h2, p, text)
import Keyboard.Extra
import PageVisibility exposing (Visibility)
import Sound
import Textures exposing (textures)
import Types exposing (..)
import View.Html


constants =
    { moveSpeed = 5
    , throwSpeed = 1.0
    , hatSpeed = 8
    , dudeX = 18
    , outsideViewport = 23
    }



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = View.Html.view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( { stage = Menu
      , level = 0
      , costs = 0
      , queue = []
      , spawnSpeed = 1.0
      , workerSpeed = 3.0
      , dude = initDude
      , rows = Array.empty
      , pressedKeys = []
      , hats = []
      , time = 0
      , nextSpawn = 0
      , screen = ( 800, 600 )

      -- 4/3 screen size -> 40 x 30 game units
      -- check with Camera.getViewSize model.screen model.camera
      , camera = Camera.fixedArea (40 * 30) ( 0, 0 )
      , resources = Resources.init
      , visible = True
      , stats = { workers = 0, withoutHats = 0 }
      }
        |> loadLevel 0
    , Cmd.batch [ Textures.load Resources ResourcesErr ]
    )


initDude : Dude
initDude =
    { x = constants.dudeX
    , y = Common.rowToY 0
    , animate = Idle
    , row = 0
    , hasHat = True
    }


loadLevel : Int -> Model -> Model
loadLevel index model =
    let
        model_ =
            resetLevel model

        level : List Int -> Float -> Float -> Model
        level list spawnSpeed moveSpeed =
            { model_
                | stage = BeforeLevel
                , level = index
                , queue = list
                , spawnSpeed = spawnSpeed
                , workerSpeed = moveSpeed
            }
    in
    case index of
        0 ->
            level [ 0, 1, 2, 1, 0 ] 3 3

        1 ->
            level [ 1, 2, 0, 1, 0, 1 ] 3 3.5

        2 ->
            level [ 2, 3, 1, 2, 0, 0, 3 ] 3 3.5

        3 ->
            level [ 0, 1, 3, 0, 4, 4, 3, 4 ] 2.5 4

        4 ->
            level [ 1, 2, 3, 2, 0, 1, 0, 2 ] 2.5 5

        5 ->
            level [ 4, 2, 3, 3, 1, 0, 4, 1 ] 2.5 5

        _ ->
            { model_ | stage = GameOver }


resetLevel : Model -> Model
resetLevel model =
    { model
        | stage = Menu
        , rows = Array.empty
        , queue = []
        , time = 0
        , nextSpawn = 0
        , hats = []
    }


newWorker : Int -> Float -> Worker
newWorker row speed =
    { x = -1 * constants.outsideViewport
    , y = Common.rowToY row
    , lifetime = 0
    , moveSpeed = speed
    , hasHat = False
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.stage ) of
        ( Resources msg, _ ) ->
            ( { model | resources = Resources.update msg model.resources }
            , Cmd.none
            )

        ( ResourcesErr url, _ ) ->
            let
                _ =
                    Debug.log "Could not load texture " url
            in
            ( model, Cmd.none )

        ( LoadLevel number, _ ) ->
            ( loadLevel number model, Cmd.none )

        ( StartLevel, _ ) ->
            startLevel model

        ( KeyMsg keyMsg, _ ) ->
            { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
            }
                |> handleMenuKeys

        ( TogglePause, _ ) ->
            togglePause model

        ( PageVisible visibility, _ ) ->
            ( { model | visible = visibility == PageVisibility.Visible }
            , Cmd.none
            )

        ( Tick delta, PlayLevel ) ->
            gameTick (Keyboard.Extra.arrows model.pressedKeys) delta model

        ( Restart, _ ) ->
            init

        rest ->
            --let _ = Debug.log "ignoring" rest in
            ( model, Cmd.none )


startLevel : Model -> ( Model, Cmd Msg )
startLevel model =
    case model.stage of
        BeforeLevel ->
            ( { model
                | stage = PlayLevel
                , dude = initDude
                , rows = Array.repeat (Helpers.max model.queue + 1) []
                , nextSpawn = 0
                , stats = { workers = 0, withoutHats = 0 }
              }
            , Cmd.batch [ Sound.play "whistle", Sound.loop "factory" ]
            )

        _ ->
            ( model, Cmd.none )


handleMenuKeys : Model -> ( Model, Cmd Msg )
handleMenuKeys model =
    case model.stage of
        PauseLevel ->
            if
                List.any
                    (\key ->
                        key == Keyboard.Extra.Escape || key == Keyboard.Extra.Enter
                    )
                    model.pressedKeys
            then
                togglePause model
            else
                ( model, Cmd.none )

        PlayLevel ->
            if keyPressed Keyboard.Extra.Escape model then
                togglePause model
            else
                ( model, Cmd.none )

        BeforeLevel ->
            if keyPressed Keyboard.Extra.Enter model then
                startLevel model
            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


keyPressed : Keyboard.Extra.Key -> { b | pressedKeys : List Keyboard.Extra.Key } -> Bool
keyPressed key { pressedKeys } =
    --List.any ((==) key) model.pressedKeys
    List.member key pressedKeys


togglePause : Model -> ( Model, Cmd msg )
togglePause model =
    case model.stage of
        PauseLevel ->
            ( { model | stage = PlayLevel }, Sound.mute False )

        PlayLevel ->
            ( { model | stage = PauseLevel }, Sound.mute True )

        _ ->
            ( model, Cmd.none )


gameTick : Keyboard.Extra.Arrows -> Float -> Model -> ( Model, Cmd Msg )
gameTick arrows delta model =
    { model | time = model.time + delta, nextSpawn = model.nextSpawn - delta }
        |> updateDude arrows delta
        |> maybeThrow arrows
        |> updateRows delta
        |> updateHats delta
        |> spawnNewWorker
        |> checkLevelEnd


updateDude : Keyboard.Extra.Arrows -> Float -> Model -> ( Model, Cmd Msg )
updateDude arrows delta model =
    let
        ( dude, cmd ) =
            moveDude delta arrows model.dude
    in
    ( { model | dude = dude }, cmd )


moveDude : Float -> { a | y : Int } -> Dude -> ( Dude, Cmd Msg )
moveDude delta { y } ({ row } as dude) =
    case dude.animate of
        MoveUp destination ->
            let
                y =
                    dude.y + (constants.moveSpeed * delta)
            in
            if y >= destination then
                ( { dude | y = destination, animate = Idle }, Cmd.none )
            else
                ( { dude | y = y }, Cmd.none )

        MoveDown destination ->
            let
                y =
                    dude.y - (constants.moveSpeed * delta)
            in
            if y <= destination then
                ( { dude | y = destination, animate = Idle }, Cmd.none )
            else
                ( { dude | y = y }, Cmd.none )

        Throw time ->
            if time > 0 then
                ( { dude | animate = Throw (time - delta) }, Cmd.none )
            else
                ( { dude | animate = GetHat }, Sound.play "buy" )

        GetHat ->
            if dude.x < constants.outsideViewport then
                ( { dude
                    | x = dude.x + (constants.moveSpeed * delta)
                    , animate = GetHat
                  }
                , Cmd.none
                )
            else
                -- emit ka-ching sound?
                ( { dude
                    | animate = ReturnWithHat
                    , hasHat = True
                  }
                , Cmd.none
                )

        ReturnWithHat ->
            ( { dude
                | x = dude.x - (constants.moveSpeed * delta)
                , animate =
                    if dude.x > constants.dudeX then
                        ReturnWithHat
                    else
                        Idle
              }
            , Cmd.none
            )

        Idle ->
            if y == 1 && row > 0 then
                ( { dude
                    | animate = MoveUp <| Common.rowToY (row - 1)
                    , row = row - 1
                  }
                , Sound.play "walk"
                )
            else if y == -1 && row < 4 then
                ( { dude
                    | animate = MoveDown <| Common.rowToY (row + 1)
                    , row = row + 1
                  }
                , Sound.play "walk"
                )
            else
                ( dude, Cmd.none )


maybeThrow : Keyboard.Extra.Arrows -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeThrow { x } ( { dude } as model, cmd ) =
    if model.dude.animate == Idle && x == -1 then
        ( { model
            | dude = { dude | animate = Throw 0.3, hasHat = False }
            , hats = Hat model.dude.row model.dude.x :: model.hats
          }
        , Cmd.batch [ Sound.play "throw", cmd ]
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
    in
    ( { model
        | rows = Array.fromList rows
        , stats = updateStats stoppedWorkers model.stats
      }
    , if List.isEmpty stoppedWorkers then
        cmd
      else
        Cmd.batch [ Sound.play "start-working", cmd ]
    )


updateRow : Float -> List Worker -> ( List Worker, List Worker )
updateRow delta workers =
    workers
        |> List.map (moveWorker delta)
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


updateStats : List Worker -> LevelStats -> LevelStats
updateStats stoppedWorkers stats =
    { stats
        | workers = stats.workers + List.length stoppedWorkers
        , withoutHats =
            Helpers.countIf unsafeWorker stats.withoutHats stoppedWorkers
    }


unsafeWorker : Worker -> Bool
unsafeWorker worker =
    not worker.hasHat


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


hatHits : Workers -> Int -> Hat -> Maybe ( Int, Int, Int, Bool )
hatHits rows hatIndex hat =
    case Array.get hat.row rows of
        Nothing ->
            Nothing

        Just workers ->
            -- updateFirst (\worker -> worker.x <= hat.x) (\worker.hasHat = True) workers
            case
                Helpers.getFirstWithIndex
                    (\worker ->
                        worker.x >= hat.x && worker.x < (constants.dudeX + 1)
                    )
                    workers
            of
                Nothing ->
                    Nothing

                Just ( index, worker ) ->
                    Just ( hatIndex, hat.row, index, worker.hasHat )


addHatToWorker :
    ( Int, Int, Int, Bool )
    -> ( { a | hats : List Hat, rows : Workers, costs : Int }, List (Cmd Msg) )
    -> ( { a | hats : List Hat, rows : Workers, costs : Int }, List (Cmd Msg) )
addHatToWorker ( hatIndex, row, workerIndex, hasHat ) ( model, cmds ) =
    let
        ( transform, sound ) =
            if hasHat then
                ( \worker ->
                    { worker
                        | hasHat = False
                        , moveSpeed = worker.moveSpeed / 3
                    }
                , "drop-hat"
                )
            else
                ( \worker ->
                    { worker
                        | hasHat = True
                        , moveSpeed = worker.moveSpeed * 3
                    }
                , "get-hat"
                )
    in
    ( { model
        | hats = Helpers.dropFromList hatIndex model.hats
        , rows = updateWorkerAtIndex row workerIndex transform model.rows
        , costs = model.costs + 50
      }
    , Sound.play sound :: cmds
    )


updateWorkerAtIndex : Int -> Int -> (Worker -> Worker) -> Workers -> Workers
updateWorkerAtIndex row workerIndex transform =
    Helpers.updateArray row
        (\list ->
            List.indexedMap
                (\index worker ->
                    if index == workerIndex then
                        transform worker
                    else
                        worker
                )
                list
        )


spawnNewWorker : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
spawnNewWorker ( model, cmd ) =
    case ( model.stage, model.queue ) of
        ( PlayLevel, row :: queue ) ->
            if model.nextSpawn <= 0 then
                ( { model
                    | nextSpawn = model.spawnSpeed
                    , queue = queue
                    , rows =
                        Helpers.updateArray row
                            (spawnWorker row model.workerSpeed)
                            model.rows
                  }
                , Cmd.batch [ cmd, Sound.play "spawn" ]
                )
            else
                ( model, cmd )

        _ ->
            ( model, cmd )


spawnWorker : Int -> Float -> List Worker -> List Worker
spawnWorker row speed workers =
    List.append workers [ newWorker row speed ]


checkLevelEnd : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkLevelEnd ( model, cmd ) =
    case ( model.stage, model.queue ) of
        ( PlayLevel, [] ) ->
            if model.dude.animate == Idle && noMoreWorkers model.rows then
                ( loadLevel (model.level + 1) model
                , Cmd.batch [ cmd, Sound.play "whistle", Sound.stop "factory" ]
                )
            else
                ( model, cmd )

        _ ->
            ( model, cmd )


noMoreWorkers : Workers -> Bool
noMoreWorkers rows =
    (List.length <| List.concat <| Array.toList rows) == 0



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.append
            (case ( model.visible, model.stage ) of
                ( True, PlayLevel ) ->
                    [ AnimationFrame.diffs (Tick << (\d -> d / 1000)) ]

                _ ->
                    []
            )
            [ Sub.map KeyMsg Keyboard.Extra.subscriptions
            , PageVisibility.visibilityChanges PageVisible
            ]
