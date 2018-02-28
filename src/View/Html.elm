module View.Html exposing (view)

import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)
import View.WebGl


view : Model -> Html Msg
view model =
    div [] [ View.WebGl.render model, overlay model ]


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


overlayWrapper : List (Html msg) -> Html msg
overlayWrapper children =
    div [ class "overlay" ] [ div [ class "modal" ] children ]


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



---- HELPERS ----


line : String -> Html msg
line string =
    p [] [ text string ]


note : String -> Html msg
note text_ =
    p [] [ Html.em [] [ text text_ ] ]
