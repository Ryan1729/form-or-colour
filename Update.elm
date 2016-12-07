module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (..)
import Extras
import Random.Pcg as Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame playerColouring ->
            ( Model.defaultModel playerColouring, Cmd.none )

        Place boardId ->
            case model.selected of
                Just piece ->
                    let
                        newModel =
                            { model
                                | board = Model.place piece boardId model.board
                                , rack = Model.removeFromRack piece model.rack
                                , selected = Nothing
                            }
                    in
                        advanceTurn newModel

                Nothing ->
                    ( model, Cmd.none )

        Select piece ->
            ( { model | selected = Just piece }, Cmd.none )

        Flip boardId ->
            let
                newModel =
                    applyFlipMove model boardId
            in
                advanceTurn newModel

        SetWidth width ->
            ( { model | width = width }, Cmd.none )


advanceTurn : Model -> ( Model, Cmd Msg )
advanceTurn newModel =
    if isCPULosingModel newModel then
        ( { newModel | gameState = Win }, Cmd.none )
    else if isUserLosingModel newModel then
        ( { newModel | gameState = Loss }, Cmd.none )
    else
        ( cpuTurn newModel, Cmd.none )


type Move
    = PlaceMove ( Piece, BoardId )
    | FlipMove BoardId


getMoves : Colouring -> Rack -> Board -> List Move
getMoves colouring rack board =
    let
        boardIds =
            Model.getAvailableBoardIds board

        placeMoves =
            List.concatMap
                (\piece ->
                    List.map ((,) piece)
                        boardIds
                )
                (Model.getAvailablePieces colouring rack)
                |> List.map PlaceMove

        flipMoves =
            --TODO filter out other colour
            getUsedBoardIds board
                |> List.map FlipMove
    in
        (placeMoves ++ flipMoves)
            |> Extras.shuffle (Random.initialSeed 42)


isCPULosingModel : Model -> Bool
isCPULosingModel model =
    False


isUserLosingModel : Model -> Bool
isUserLosingModel model =
    False


nextPlayerHasNoWinningMove : Model -> Move -> Bool
nextPlayerHasNoWinningMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves model.playerColouring model.rack potentialModel.board
    in
        case Extras.find (userWinningMove potentialModel) potentialFutureMoves of
            Just _ ->
                False

            Nothing ->
                True


userWinningMove : Model -> Move -> Bool
userWinningMove model move =
    applyMove model move
        |> isCPULosingModel


cpuWinningMove : Model -> Move -> Bool
cpuWinningMove model move =
    applyMove model move
        |> isUserLosingModel


cpuTurn : Model -> Model
cpuTurn model =
    let
        moves : List Move
        moves =
            getMoves (Model.oppositeColouring model.playerColouring) model.rack model.board

        postMovementModel =
            Extras.find (cpuWinningMove model) moves
                |> Extras.orElseLazy (\() -> Extras.find (nextPlayerHasNoWinningMove model) moves)
                |> Extras.orElseLazy (\() -> Random.step (Random.sample moves) (Random.initialSeed 42) |> Tuple.first)
                |> Maybe.map (applyMove model)
                |> Maybe.withDefault model
    in
        if isCPULosingModel postMovementModel then
            { postMovementModel | gameState = Win }
        else if isUserLosingModel postMovementModel then
            { postMovementModel | gameState = Loss }
        else
            postMovementModel


applyMove : Model -> Move -> Model
applyMove model move =
    case move of
        PlaceMove pm ->
            applyPlaceMove model pm

        FlipMove fm ->
            applyFlipMove model fm


applyPlaceMove : Model -> ( Piece, BoardId ) -> Model
applyPlaceMove model ( piece, boardId ) =
    { model
        | board = Model.place piece boardId model.board
        , rack = Model.removeFromRack piece model.rack
    }


applyFlipMove : Model -> BoardId -> Model
applyFlipMove model boardId =
    { model | board = Model.flipBoardPiece boardId model.board }
