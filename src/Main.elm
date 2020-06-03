module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import TicTacToe exposing (Board, Game, GameState(..), Player(..), cellAt, selectCell, startGame)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Game


init : Model
init =
    startGame



-- UPDATE


type Msg
    = Change Int


update : Msg -> Model -> Model
update (Change pos) game =
    selectCell pos game



-- VIEW
-- style="display:flex;justify-content:center;align-items:center;"


symbolForPlayer : Player -> String
symbolForPlayer player =
    case player of
        Cross ->
            "x"

        Circle ->
            "o"

        _ ->
            ""


buildCell : Board -> Int -> Html Msg
buildCell board pos =
    div
        [ css
            [ display inlineBlock
            , verticalAlign middle
            , textAlign center
            , boxSizing borderBox
            , width (px 100)
            , height (pct 100)
            , fontSize (px 50)
            , fontWeight bold
            , border3 (px 2) solid (rgb 120 120 120)
            , hover
                [ borderColor (rgb 255 0 0)
                , borderRadius (px 10)
                ]
            ]
        , onClick (Change pos)
        ]
        [ text <| symbolForPlayer (cellAt board pos)
        ]


buildRow : Board -> Int -> Html Msg
buildRow board j =
    div
        [ css
            [ height (px 100)
            , lineHeight (px 100)
            ]
        ]
        (List.range 0 2
            |> List.map (\i -> (j * 3) + i)
            |> List.map (buildCell board)
        )


buildGrid : Board -> Html Msg
buildGrid board =
    div
        []
        (List.range 0 2
            |> List.map (buildRow board)
        )


buildPage : Model -> Html Msg
buildPage game =
    case game.state of
        Running ->
            div []
                [ text ("Game running with player turn " ++ symbolForPlayer game.player)
                , buildGrid game.board
                ]

        Won player ->
            div []
                [ text (symbolForPlayer player ++ " won")
                , buildGrid game.board
                ]

        Draw ->
            div []
                [ text "Draw"
                , buildGrid game.board
                ]


view : Model -> Html.Html Msg
view model =
    buildPage model |> toUnstyled
