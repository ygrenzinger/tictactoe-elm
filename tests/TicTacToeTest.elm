module TicTacToeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, map)
import Random.List exposing (shuffle)
import Shrink exposing (noShrink)
import Test exposing (..)
import TicTacToe exposing (Board, Game, GameState(..), selectCell, showGame, startGame, stringToGame)


initGameAndSelectCell : String -> Int -> String
initGameAndSelectCell init pos =
    stringToGame init |> selectCell pos |> showGame


runGame : List Int -> Game
runGame =
    List.foldr selectCell startGame


fuzzPositions : Fuzzer (List Int)
fuzzPositions =
    Fuzz.custom (shuffle (List.range 0 8)) noShrink


fuzzGame : Fuzzer Game
fuzzGame =
    map runGame fuzzPositions


suite : Test
suite =
    describe "Tic Tac Toe"
        [ test "Should start a game" <|
            \_ -> Expect.equal "Running - Player X - _________" (showGame startGame)
        , test "Should current player select a cell and switch player" <|
            \_ -> Expect.equal "Running - Player O - _X_______" (startGame |> selectCell 1 |> showGame)
        , test "Should make play Cross" <|
            \_ -> Expect.equal "Running - Player X - _XO______" (initGameAndSelectCell "_X_______" 2)
        , test "Should not change a select cell" <|
            \_ -> Expect.equal "Running - Player X - _XO______" (initGameAndSelectCell "_XO______" 2)
        , test "Should make player Cross with row" <|
            \_ -> Expect.equal "X won - Player X - XXXOO____" (initGameAndSelectCell "XX_OO____" 2)
        , test "Should make player Cross with column" <|
            \_ -> Expect.equal "X won - Player X - X_OXO_X__" (initGameAndSelectCell "X_OXO____" 6)
        , test "Should make player Circle win" <|
            \_ -> Expect.equal "O won - Player O - OXXXO___O" (initGameAndSelectCell "OXXXO____" 8)
        , test "Should end with a draw if there is no winner and the board is full" <|
            \_ -> Expect.equal "Draw - Player X - XOXXOOOXX" (initGameAndSelectCell "XOXXOOOX_" 8)
        , fuzz fuzzGame "End state" <|
            \game ->
                let
                    notRunning =
                        case game.state of
                            Running ->
                                False

                            _ ->
                                True
                in
                Expect.true "end state is not running" notRunning
        ]
