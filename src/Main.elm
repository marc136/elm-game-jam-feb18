module Main exposing (..)

import AnimationFrame
import Array exposing (Array)
import Array.Extra
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Helpers
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Keyboard.Extra
import PageVisibility exposing (Visibility)
import Sound
import Textures exposing (textures)


constants =
    { moveSpeed = 5
    , throwSpeed = 1.0
    , hatSpeed = 8
    , dudeX = 18
    , outsideViewport = 23
    }



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
    , y = rowToY 0
    , animate = Idle
    , row = 0
    , hasHat = True
    }


rowToY : Int -> Float
rowToY row =
    13 - (toFloat row * 5)


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
            level [ 0, 3, 1, 3 ] 3 3

        1 ->
            level [ 1, 2, 0, 1, 0, 1, 3 ] 3 3.5

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
    , y = rowToY row
    , lifetime = 0
    , moveSpeed = speed
    , hasHat = False
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
                    | animate = MoveUp <| rowToY (row - 1)
                    , row = row - 1
                  }
                , Sound.play "walk"
                )
            else if y == -1 && row < 4 then
                ( { dude
                    | animate = MoveDown <| rowToY (row + 1)
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
                        Array.Extra.update row
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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Game.render (renderConfig model) (toRenderables model)
        , overlay model
        ]


overlay : Model -> Html Msg
overlay model =
    case model.stage of
        BeforeLevel ->
            overlayWrapper <| beforeLevel model

        PauseLevel ->
            overlayWrapper
                [ h1 [] [ text "Game is paused" ]
                , note "Press [Escape] or [Enter] to continue"
                , button [ onClick TogglePause ] [ text "continue" ]
                ]

        GameOver ->
            overlayWrapper <|
                [ h1 [] [ text "You were fired!" ]
                , line <| "After " ++ toString model.level ++ " days without an accident, a worker was injured today."
                , line <| "And someone in management calculated that it is cheaper to pay for the work accidents instead of paying your salary and the expenses of " ++ toString model.costs ++ "€ for safety equipment."
                , line "Business is not fair..."
                , button [ onClick Restart ] [ text "Try again." ]
                ]

        _ ->
            text ""


line : String -> Html msg
line string =
    p [] [ text string ]


note : String -> Html msg
note text_ =
    p [] [ Html.em [] [ text text_ ] ]


overlayWrapper : List (Html msg) -> Html msg
overlayWrapper children =
    div [ class "overlay" ]
        [ div [ class "modal" ] children ]


beforeLevel : Model -> List (Html Msg)
beforeLevel ({ stats, level } as model) =
    case level of
        0 ->
            beforeFirstLevel

        1 ->
            afterFirstLevel model

        _ ->
            [ h2 [] [ text "Your shift has ended" ]
            , h2 []
                [ Html.em [] [ text <| toString level ]
                , text " days without accident"
                ]
            , p []
                [ text <|
                    if stats.withoutHats > 0 then
                        "Even though " ++ toString stats.withoutHats ++ " workers were not properly equipped."
                    else
                        "But this was to be expected because you managed to provide all " ++ toString stats.workers ++ " workers with the proper safety equipment."
                ]
            , button [ onClick <| StartLevel ] [ text "Sleep" ]
            ]


beforeFirstLevel : List (Html Msg)
beforeFirstLevel =
    [ h1 [] [ text "Welcome to Sector 7G" ]
    , line "You were hired as a safety inspector and are tasked with reducing the number of work accidents."
    , line "The longest time without an accident in this sector was three days."
    , line "Use the arrow keys to move and the left arrow key to throw a safety hat to a worker."
    , line "But beware, each hat costs money and you will not only be evaluated according to the number of accidents, but also on the costs for safety equipment."
    , note "Note: You can pause the game by pressing the Escape key."
    , button [ onClick StartLevel ] [ text "Start work" ]
    ]


afterFirstLevel : Model -> List (Html Msg)
afterFirstLevel { stats } =
    [ h1 [] [ text "You are off to a good start!" ]
    , line "Your first day at work and only because of your effort there were no accidents."
    , line <|
        if stats.withoutHats > 0 then
            "But if you are completely honest, there was a little bit of luck involved, as " ++ toString stats.withoutHats ++ " workers went to their jobs without proper safety equipment."
        else
            "But this was expected because you did an awesome job and distributed proper safety equipment to all " ++ toString stats.workers ++ " workers."
    , line "Keep up the good work tomorrow, too."
    , button [ onClick StartLevel ] [ text "Sleep" ]
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
            (case ( model.visible, model.stage ) of
                ( True, PlayLevel ) ->
                    [ AnimationFrame.diffs (Tick << (\d -> d / 1000))

                    --, Window.resizes ScreenSize
                    ]

                _ ->
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
