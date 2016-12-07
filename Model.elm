module Model exposing (..)


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , rack : Rack
    , gameState : GameState
    , width : Float
    , playerColouring : Colouring
    }


defaultModel playerColouring =
    { board = initialBoard
    , selected = Just (Piece playerColouring X)
    , rack = initialRack
    , gameState = InProgress
    , width = 768
    , playerColouring = playerColouring
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


oppositeSymbol : Symbol -> Symbol
oppositeSymbol s =
    case s of
        X ->
            O

        O ->
            X


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


getFlippapleBoardIds : Colouring -> Board -> List BoardId
getFlippapleBoardIds colouring board =
    boardIdPossibilities
        |> List.filter
            (\id ->
                case getSpace id board of
                    EmptySpace ->
                        False

                    Space (Piece spaceColouring _) ->
                        spaceColouring == colouring
            )


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


flipBoardPiece : BoardId -> Board -> Board
flipBoardPiece boardId board =
    setSpace boardId (getSpace boardId board |> flipSpace) board


flipSpace : Space -> Space
flipSpace space =
    case space of
        Space (Piece colouring symbol) ->
            Piece colouring (oppositeSymbol symbol) |> Space

        EmptySpace ->
            EmptySpace


lineExists : Colouring -> Board -> Bool
lineExists colouring board =
    checkLine colouring board ZeroZero OneZero TwoZero ThreeZero
        || checkLine colouring board ZeroOne OneOne TwoOne ThreeOne
        || checkLine colouring board ZeroTwo OneTwo TwoTwo ThreeTwo
        || checkLine colouring board ZeroThree OneThree TwoThree ThreeThree
        || checkLine colouring board ZeroZero ZeroOne ZeroTwo ZeroThree
        || checkLine colouring board OneZero OneOne OneTwo OneThree
        || checkLine colouring board TwoZero TwoOne TwoTwo TwoThree
        || checkLine colouring board ThreeZero ThreeOne ThreeTwo ThreeThree
        || checkLine colouring board ZeroZero OneOne TwoTwo ThreeThree
        || checkLine colouring board ThreeZero TwoOne OneTwo ZeroThree


checkLine : Colouring -> Board -> BoardId -> BoardId -> BoardId -> BoardId -> Bool
checkLine colouring board id1 id2 id3 id4 =
    case colouring of
        --check for lines for the form player
        Plain ->
            symbolsMatch board id1 id2
                && symbolsMatch board id2 id3
                && symbolsMatch board id3 id4

        --check for lines for the colour player
        Coloured ->
            lineColoursMatch board id1 id2
                && lineColoursMatch board id2 id3
                && lineColoursMatch board id3 id4


symbolsMatch : Board -> BoardId -> BoardId -> Bool
symbolsMatch board id1 id2 =
    case ( getSpace id1 board, getSpace id2 board ) of
        ( Space (Piece _ s1), Space (Piece _ s2) ) ->
            s1 == s2

        _ ->
            False


lineColoursMatch : Board -> BoardId -> BoardId -> Bool
lineColoursMatch board id1 id2 =
    case ( getSpace id1 board, getSpace id2 board ) of
        ( Space p1, Space p2 ) ->
            lineColourFromPiece p1 == lineColourFromPiece p2

        _ ->
            False


type LineColour
    = Dark
    | Light


lineColourFromPiece : Piece -> LineColour
lineColourFromPiece piece =
    case piece of
        Piece Plain X ->
            Dark

        Piece Plain O ->
            Light

        Piece Coloured X ->
            Light

        Piece Coloured O ->
            Dark


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
