module Tests exposing (..)

import Array
import Expect
import Main exposing (..)
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


findFirstTest : Test
findFirstTest =
    describe "findFirst"
        [ test "second matches" <|
            \_ ->
                findFirst identity [ False, True, True ]
                    |> Expect.equal (Just 1)
        , test "tenth matches" <|
            \_ ->
                findFirst identity (List.append (List.repeat 9 False) [ True ])
                    |> Expect.equal (Just 9)
        , test "none match" <|
            \_ ->
                findFirst identity [ False, False ]
                    |> Expect.equal Nothing
        , test "empty list" <|
            \_ ->
                findFirst identity []
                    |> Expect.equal Nothing
        ]


hatTest : Test
hatTest =
    let
        worker : Worker
        worker =
            Main.newWorker 1

        rows =
            Array.fromList
                [ []
                , [ { worker | x = 8 }, { worker | x = 9 }, { worker | x = 10 } ]
                , []
                , [ { worker | x = 4 } ]
                , []
                ]

        hit : Hat -> Maybe ( Int, Int, Int )
        hit =
            hatHits rows 0
    in
    describe "Test for hitting hats"
        [ test "first matches" <|
            \_ ->
                hit { x = 5, row = 1 }
                    |> Expect.equal (Just ( 0, 1, 0 ))
        , test "third matches exactly" <|
            \_ ->
                hit { x = 10, row = 1 }
                    |> Expect.equal (Just ( 0, 1, 2 ))
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
                    |> Expect.equal (Just ( 0, 3, 0 ))
        ]
