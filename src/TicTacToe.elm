module TicTacToe exposing (..)

import Array exposing (Array)


type Player
    = Cross
    | Circle
    | None


type alias Board =
    Array Player


type GameState
    = Running
    | Won Player
    | Draw


type alias Game =
    { state : GameState
    , player : Player
    , board : Board
    }


showPlayer : Player -> String
showPlayer player =
    case player of
        Cross ->
            "X"

        Circle ->
            "O"

        None ->
            "_"


showGameState : GameState -> String
showGameState state =
    case state of
        Running ->
            "Running"

        Draw ->
            "Draw"

        Won player ->
            showPlayer player ++ " won"


showBoard : Board -> String
showBoard board =
    board |> Array.toList |> List.map showPlayer |> String.concat


showGame : Game -> String
showGame game =
    List.intersperse " - " [ showGameState game.state, "Player " ++ showPlayer game.player, showBoard game.board ]
        |> String.concat


stringToPlayer : Char -> Player
stringToPlayer s =
    case s of
        'X' ->
            Cross

        'O' ->
            Circle

        _ ->
            None


stringToBoard : String -> Board
stringToBoard =
    Array.fromList << List.map stringToPlayer << String.toList


currentPlayer : Board -> Player
currentPlayer board =
    let
        xs =
            Array.filter ((==) Cross) board |> Array.length

        ys =
            Array.filter ((==) Circle) board |> Array.length
    in
    if xs > ys then
        Circle

    else
        Cross


stringToGame : String -> Game
stringToGame s =
    let
        board =
            stringToBoard s
    in
    initGame board


startGame : Game
startGame =
    { board = Array.repeat 9 None
    , player = Cross
    , state = Running
    }


initGame : Board -> Game
initGame board =
    { board = board
    , player = currentPlayer board
    , state = gameState board
    }


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        Cross ->
            Circle

        Circle ->
            Cross

        None ->
            None


cellAt : Board -> Int -> Player
cellAt board pos =
    Maybe.withDefault None <| Array.get pos board


winningPositions =
    [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ], [ 0, 4, 8 ], [ 2, 4, 6 ] ]


hasPlayerWon : Board -> Player -> Bool
hasPlayerWon board player =
    List.filter (\positions -> List.map (cellAt board) positions |> List.all ((==) player)) winningPositions
        |> List.isEmpty
        |> not


hasWinner : Board -> Player
hasWinner board =
    if hasPlayerWon board Cross then
        Cross

    else if hasPlayerWon board Circle then
        Circle

    else
        None


isFull : Board -> Bool
isFull =
    Array.isEmpty << Array.filter ((==) None)


gameState : Board -> GameState
gameState board =
    case hasWinner board of
        None ->
            if isFull board then
                Draw

            else
                Running

        winner ->
            Won winner


playTurn : Int -> Game -> Game
playTurn pos game =
    let
        updatedBoard =
            Array.set pos game.player game.board

        state =
            gameState updatedBoard
    in
    if state == Running then
        { player = switchPlayer game.player
        , state = Running
        , board = updatedBoard
        }

    else
        { player = game.player
        , state = state
        , board = updatedBoard
        }


selectCell : Int -> Game -> Game
selectCell pos game =
    let
        alreadySelected =
            case cellAt game.board pos of
                None ->
                    False

                _ ->
                    True
    in
    if alreadySelected || not (game.state == Running) then
        game

    else
        playTurn pos game
