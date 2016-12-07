module Model exposing (..)


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , rack : Rack
    , gameState : GameState
    , width : Float
    , playerColouring : Colouring
    }


defaultModel =
    { board = initialBoard
    , selected = Just (Piece Plain X)
    , rack = initialRack
    , gameState = InProgress
    , width = 768
    , playerColouring = Plain
    }


type GameState
    = InProgress
    | Win
    | Loss


type Space
    = EmptySpace
    | Space Piece


type Piece
    = Piece Colouring Symbol


type Colouring
    = Plain
    | Coloured


oppositeColouring : Colouring -> Colouring
oppositeColouring c =
    case c of
        Plain ->
            Coloured

        Coloured ->
            Plain


type Symbol
    = X
    | O


type alias Rack =
    { plain : Int
    , coloured : Int
    }


initialRack : Rack
initialRack =
    { plain = 8
    , coloured = 8
    }


removeFromRack : Piece -> Rack -> Rack
removeFromRack piece rack =
    case piece of
        Piece Plain _ ->
            { rack | plain = max 0 (rack.plain - 1) }

        Piece Coloured _ ->
            { rack | coloured = max 0 (rack.coloured - 1) }


place : Piece -> BoardId -> Board -> Board
place piece boardId board =
    setSpace boardId (Space piece) board


getAvailableBoardIds : Board -> List BoardId
getAvailableBoardIds board =
    boardIdPossibilities
        |> List.filter (\id -> getSpace id board == EmptySpace)


getAvailablePieces : Colouring -> Rack -> List Piece
getAvailablePieces colouring rack =
    case colouring of
        Plain ->
            if rack.plain > 0 then
                [ Piece Plain O, Piece Plain X ]
            else
                []

        Coloured ->
            if rack.coloured > 0 then
                [ Piece Coloured O, Piece Coloured X ]
            else
                []


type alias Board =
    { zeroZero : Space
    , oneZero : Space
    , twoZero : Space
    , threeZero : Space
    , zeroOne : Space
    , oneOne : Space
    , twoOne : Space
    , threeOne : Space
    , zeroTwo : Space
    , oneTwo : Space
    , twoTwo : Space
    , threeTwo : Space
    , zeroThree : Space
    , oneThree : Space
    , twoThree : Space
    , threeThree : Space
    }


initialBoard : Board
initialBoard =
    { zeroZero = EmptySpace
    , oneZero = EmptySpace
    , twoZero = EmptySpace
    , threeZero = EmptySpace
    , zeroOne = EmptySpace
    , oneOne = EmptySpace
    , twoOne = EmptySpace
    , threeOne = EmptySpace
    , zeroTwo = EmptySpace
    , oneTwo = EmptySpace
    , twoTwo = EmptySpace
    , threeTwo = EmptySpace
    , zeroThree = EmptySpace
    , oneThree = EmptySpace
    , twoThree = EmptySpace
    , threeThree = EmptySpace
    }


type BoardId
    = ZeroZero
    | OneZero
    | TwoZero
    | ThreeZero
    | ZeroOne
    | OneOne
    | TwoOne
    | ThreeOne
    | ZeroTwo
    | OneTwo
    | TwoTwo
    | ThreeTwo
    | ZeroThree
    | OneThree
    | TwoThree
    | ThreeThree


boardIdPossibilities =
    [ ZeroZero
    , OneZero
    , TwoZero
    , ThreeZero
    , ZeroOne
    , OneOne
    , TwoOne
    , ThreeOne
    , ZeroTwo
    , OneTwo
    , TwoTwo
    , ThreeTwo
    , ZeroThree
    , OneThree
    , TwoThree
    , ThreeThree
    ]


getSpace : BoardId -> Board -> Space
getSpace boardId board =
    case boardId of
        ZeroZero ->
            board.zeroZero

        OneZero ->
            board.oneZero

        TwoZero ->
            board.twoZero

        ThreeZero ->
            board.threeZero

        ZeroOne ->
            board.zeroOne

        OneOne ->
            board.oneOne

        TwoOne ->
            board.twoOne

        ThreeOne ->
            board.threeOne

        ZeroTwo ->
            board.zeroTwo

        OneTwo ->
            board.oneTwo

        TwoTwo ->
            board.twoTwo

        ThreeTwo ->
            board.threeTwo

        ZeroThree ->
            board.zeroThree

        OneThree ->
            board.oneThree

        TwoThree ->
            board.twoThree

        ThreeThree ->
            board.threeThree


setSpace : BoardId -> Space -> Board -> Board
setSpace boardId space board =
    case boardId of
        ZeroZero ->
            { board | zeroZero = space }

        OneZero ->
            { board | oneZero = space }

        TwoZero ->
            { board | twoZero = space }

        ThreeZero ->
            { board | threeZero = space }

        ZeroOne ->
            { board | zeroOne = space }

        OneOne ->
            { board | oneOne = space }

        TwoOne ->
            { board | twoOne = space }

        ThreeOne ->
            { board | threeOne = space }

        ZeroTwo ->
            { board | zeroTwo = space }

        OneTwo ->
            { board | oneTwo = space }

        TwoTwo ->
            { board | twoTwo = space }

        ThreeTwo ->
            { board | threeTwo = space }

        ZeroThree ->
            { board | zeroThree = space }

        OneThree ->
            { board | oneThree = space }

        TwoThree ->
            { board | twoThree = space }

        ThreeThree ->
            { board | threeThree = space }
