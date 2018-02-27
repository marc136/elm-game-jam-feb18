module Tests exposing (..)

import Array
import Expect
import Helpers
import Main exposing (..)
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


findFirstTest : Test
findFirstTest =
    describe "findFirst"
        [ test "second matches" <|
            \_ ->
                Helpers.findFirst identity [ False, True, True ]
                    |> Expect.equal (Just 1)
        , test "tenth matches" <|
            \_ ->
                Helpers.findFirst identity
                    (List.append (List.repeat 9 False) [ True ])
                    |> Expect.equal (Just 9)
        , test "none match" <|
            \_ ->
                Helpers.findFirst identity [ False, False ]
                    |> Expect.equal Nothing
        , test "empty list" <|
            \_ ->
                Helpers.findFirst identity []
                    |> Expect.equal Nothing
        ]


hatTest : Test
hatTest =
    let
        worker : Worker
        worker =
            Main.newWorker 1 1

        rows =
            Array.fromList
                [ []
                , [ { worker | x = 8 }, { worker | x = 9 }, { worker | x = 10 } ]
                , []
                , [ { worker | x = 4 } ]
                , []
                ]

        hit : Hat -> Maybe ( Int, Int, Int, Bool )
        hit =
            hatHits rows 0
    in
    describe "Test for hitting hats"
        [ test "first matches" <|
            \_ ->
                hit { x = 5, row = 1 }
                    |> Expect.equal (Just ( 0, 1, 0, False ))
        , test "third matches exactly" <|
            \_ ->
                hit { x = 10, row = 1 }
                    |> Expect.equal (Just ( 0, 1, 2, False ))
        , test "none match" <|
            \_ ->
                hit { x = 11, row = 1 }
                    |> Expect.equal Nothing
        , test "empty row" <|
            \_ ->
                hit { x = -100, row = 0 }
                    |> Expect.equal Nothing
        , test "row does not exist" <|
            \_ ->
                hit { x = -100, row = 10 }
                    |> Expect.equal Nothing
        , test "single element matches" <|
            \_ ->
                hit { x = 0, row = 3 }
                    |> Expect.equal (Just ( 0, 3, 0, False ))
        ]


addHatToWorkerTest : Test
addHatToWorkerTest =
    let
        before =
            { hats = [ Hat 0 1 ]
            , rows =
                Array.fromList
                    [ [ newWorker 0 1 ]
                    , [ newWorker 1 1, newWorker 1 1 ]
                    , []
                    ]
            , costs = 0
            }

        addHat data =
            addHatToWorker data ( before, [ Cmd.none ] )
                |> Tuple.first

        getWorker ( _, row, index, _ ) validate model =
            case Array.get row model.rows of
                Nothing ->
                    Expect.fail "Row not found"

                Just workers ->
                    case List.head <| List.drop index workers of
                        Nothing ->
                            Expect.fail "Worker not found"

                        Just worker ->
                            Expect.equal True (validate worker)

        assert caption data validateWorker =
            test caption <|
                \_ ->
                    addHat data
                        |> Expect.all
                            [ \{ hats } -> Expect.equal [] hats
                            , getWorker data validateWorker
                            ]
    in
    describe "addHatToWorkerTest"
        [ assert "give a hat to the first worker in row 0"
            ( 0, 0, 0, False )
            (\worker -> worker.hasHat)
        , assert "remove the hat of the first worker in row 0"
            ( 0, 0, 0, True )
            (\worker -> not worker.hasHat)
        , assert "give a hat to the 2nd worker in row 1"
            ( 0, 1, 1, False )
            .hasHat
        ]
