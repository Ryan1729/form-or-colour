module Model exposing (..)


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , rack : Rack
    , gameState : GameState
    }


defaultModel =
    { board = initialBoard
    , selected = Nothing
    , rack = initialRack
    , gameState = InProgress
    }


type GameState
    = InProgress
    | Win
    | Loss


type alias Board =
    {}


initialBoard : Board
initialBoard =
    {}


type Piece
    = Piece Colouring Symbol


type Colouring
    = Plain
    | Coloured


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


type BoardId
    = BoardId


place : Piece -> BoardId -> Board -> Board
place piece boardId board =
    board


getAvailableBoardIds : Board -> List BoardId
getAvailableBoardIds board =
    []


getAvailablePieces : Rack -> List Piece
getAvailablePieces rack =
    []
