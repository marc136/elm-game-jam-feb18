module Main exposing (..)

import AnimationFrame
import Array exposing (Array)
import Array.Extra
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, div, h1, img, text)
import Keyboard.Extra
import PageVisibility exposing (Visibility)
import Ports
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
    , rows : Array (List Worker)
    , angryWorkers : List Worker
    , happyWorkers : List Worker
    , hats : List Hat
    , timeUntilNextThrow : Float
    , visible : Bool
    }


type alias Hat =
    { row : Int
    , x : Float
    }


type Animate
    = Idle
    | MoveUp Float
    | MoveDown Float


constants =
    { moveSpeed = 5
    , throwSpeed = 1.0
    , hatSpeed = 8
    }


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
    { x = -23
    , y = rowToY row
    , lifetime = 0
    , moveSpeed = 1.5
    , hasHat = False
    }



---- INIT ----


init : ( Model, Cmd Msg )
init =
    ( { dude = initDude
      , rows = Array.fromList [ [], [ newWorker 1 ] ]
      , angryWorkers = []
      , happyWorkers = []
      , pressedKeys = []
      , hats = []
      , time = 0
      , timeUntilNextThrow = 0
      , screen = ( 800, 600 )

      -- 4/3 screen size -> 40 x 30 game units
      -- check with Camera.getViewSize model.screen model.camera
      , camera = Camera.fixedArea (40 * 30) ( 0, 0 )
      , resources = Resources.init
      , visible = True
      }
    , Cmd.batch
        [ loadTextures
            [ textures.dude
            , textures.worker
            , textures.background
            , textures.hat
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
    , hat = "images/hat.png"
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
    | PageVisible Visibility


workerKeepsWalking : Worker -> Bool
workerKeepsWalking worker =
    worker.x < 15


isHappyWorker : Worker -> Bool
isHappyWorker worker =
    worker.hasHat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )

        PageVisible visibility ->
            ( { model | visible = visibility == PageVisibility.Visible }
            , Cmd.none
            )


gameTick : Keyboard.Extra.Arrows -> Float -> Model -> ( Model, Cmd Msg )
gameTick arrows delta model =
    { model
        | time = model.time + delta
        , timeUntilNextThrow = model.timeUntilNextThrow - delta
    }
        |> updateDude arrows delta
        |> maybeThrow arrows
        |> updateRows delta
        |> updateHats delta


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
moveDude dt dude =
    case dude.animate of
        MoveUp destination ->
            let
                y =
                    dude.y + (constants.moveSpeed * dt)
            in
            if y >= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        MoveDown destination ->
            let
                y =
                    dude.y - (constants.moveSpeed * dt)
            in
            if y <= destination then
                { dude | y = destination, animate = Idle }
            else
                { dude | y = y }

        _ ->
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
maybeThrow { x } ( model, cmd ) =
    if x == -1 && model.timeUntilNextThrow <= 0 then
        ( { model
            | timeUntilNextThrow = constants.throwSpeed
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
        | x =
            if worker.x < 15 then
                worker.x + (worker.moveSpeed * delta)
            else
                worker.x
        , lifetime = worker.lifetime + delta
    }


updateHats : Float -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateHats delta ( model, cmd ) =
    case
        List.indexedMap (hatHits model.rows) model.hats
            |> List.filterMap identity
    of
        [ ( hatIndex, row, workerIndex ) ] ->
            ( { model
                | hats = dropFromList hatIndex model.hats
                , rows =
                    Array.Extra.update row
                        (\workers ->
                            List.indexedMap
                                (\index worker ->
                                    { worker | hasHat = True }
                                )
                                workers
                        )
                        model.rows
              }
            , cmd
            )

        _ ->
            ( { model
                | hats = List.map (moveHat delta) model.hats
              }
            , cmd
            )


moveHat : Float -> Hat -> Hat
moveHat delta hat =
    { hat | x = hat.x - delta * constants.hatSpeed }


dropFromList : Int -> List a -> List a
dropFromList index list =
    list
        |> List.indexedMap
            (\index_ hat ->
                if index == index_ then
                    Nothing
                else
                    Just hat
            )
        |> List.filterMap identity


hatHits : Array (List Worker) -> Int -> Hat -> Maybe ( Int, Int, Int )
hatHits rows hatIndex hat =
    case Array.get hat.row rows of
        Nothing ->
            Nothing

        Just workers ->
            case findFirst (\worker -> worker.x >= hat.x) workers of
                Nothing ->
                    Nothing

                Just workerIndex ->
                    Just ( hatIndex, hat.row, workerIndex )



{- }
   updateFirst (\worker -> worker.x <= hat.x) (\worker.hasHat = True) workers
-}


updateFirst : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirst check transform list =
    let
        helper before list =
            case list of
                [] ->
                    List.reverse before

                head :: tail ->
                    if check head then
                        List.append
                            (List.reverse (transform head :: before))
                            tail
                    else
                        helper (head :: before) tail
    in
    helper [] list


findFirst : (a -> Bool) -> List a -> Maybe Int
findFirst check list =
    findFirstHelper check 0 list


findFirstHelper : (a -> Bool) -> Int -> List a -> Maybe Int
findFirstHelper check index list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if check head then
                Just index
            else
                findFirstHelper check (index + 1) tail



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


row : Resources -> List Worker -> List Renderable
row resources workers =
    List.map (worker resources) workers


worker : Resources -> Worker -> Renderable
worker resources ({ x, y } as worker) =
    let
        row =
            if worker.hasHat then
                0
            else
                1

        height =
            0.1494140625
    in
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( 6, 8 )
        , texture = Resources.getTexture textures.worker resources
        , bottomLeft = ( 0, row * height )
        , topRight = ( 1, (row + 1) * height )
        , duration = 1.2
        , numberOfFrames = 4
        , rotation = 0
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


hat : Resources -> Hat -> Renderable
hat resources { x, row } =
    Render.animatedSpriteWithOptions
        { position = ( x, rowToY row, 0 )

        -- if a size value is < 0 the image is flipped
        , size = ( 3, 4 )
        , texture = Resources.getTexture textures.hat resources

        -- bottomLeft and topRight allow to select a subsection of an image
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
            (if model.visible then
                [ Sub.map KeyMsg Keyboard.Extra.subscriptions
                , AnimationFrame.diffs (Tick << (\d -> d / 1000))

                --, Window.resizes ScreenSize
                ]
             else
                []
            )
            [ PageVisibility.visibilityChanges PageVisible ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
