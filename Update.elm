module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (..)
import Extras
import Random.Pcg as Random
import Ports


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame playerColouring ->
            let
                initialModel =
                    Model.defaultModel playerColouring

                newModel =
                    if playerColouring == Plain then
                        initialModel
                    else
                        cpuTurn initialModel
            in
                ( newModel, Cmd.none )

        Place boardId ->
            if model.gameState == InProgress then
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
                            if wouldBeTie newModel then
                                ( model, alertNoTies )
                            else
                                advanceTurn newModel

                    Nothing ->
                        ( model, Cmd.none )
            else
                ( model, Cmd.none )

        Flip boardId ->
            if model.gameState == InProgress then
                let
                    newModel =
                        applyFlipMove model boardId
                in
                    if wouldBeTie newModel then
                        ( model, alertNoTies )
                    else
                        advanceTurn newModel
            else
                ( model, Cmd.none )

        Select piece ->
            ( { model | selected = Just piece }, Cmd.none )

        SetWidth width ->
            ( { model | width = width }, Cmd.none )


alertNoTies : Cmd Msg
alertNoTies =
    Ports.alert "Moves that would create a tie are not allowed"


wouldBeTie : Model -> Bool
wouldBeTie model =
    isCPULosingModel model
        && isUserLosingModel model


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


getMoves : Colouring -> Model -> List Move
getMoves colouring model =
    let
        boardIds =
            Model.getAvailableBoardIds model.board

        placeMoves =
            List.concatMap
                (\piece ->
                    List.map ((,) piece)
                        boardIds
                )
                (Model.getAvailablePieces colouring model.rack)
                |> List.map PlaceMove

        flipMoves =
            getFlippapleBoardIds colouring model.board
                |> List.map FlipMove
    in
        (placeMoves ++ flipMoves)
            |> List.filter (applyMove model >> wouldBeTie >> not)
            |> Extras.shuffle (Random.initialSeed 42)


isCPULosingModel : Model -> Bool
isCPULosingModel model =
    Model.lineExists model.playerColouring model.board


isUserLosingModel : Model -> Bool
isUserLosingModel model =
    Model.lineExists (Model.oppositeColouring model.playerColouring) model.board


nextPlayerHasNoWinningMove : Model -> Move -> Bool
nextPlayerHasNoWinningMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves model.playerColouring potentialModel
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
            getMoves (Model.oppositeColouring model.playerColouring) model

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
