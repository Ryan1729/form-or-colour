module View exposing (view)

import MaterialModel exposing (MaterialModel)
import Model exposing (..)
import Html exposing (Html, text)
import Html.App
import Html.Attributes
import MaterialMsg exposing (MaterialMsg(Mdl, U))
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, rect, path, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import PieceView


view : MaterialModel -> Html MaterialMsg
view { mdl, model } =
    Html.div []
        [ Button.render Mdl
            [ 0 ]
            mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick (U NewGame)
            ]
            [ text "New Game" ]
        , Grid.grid []
            [ Grid.cell [ Grid.size All 5 ]
                [ PieceView.renderRack model.selected model.rack
                ]
            , Grid.cell [ Grid.size All 6 ]
                [ Html.div [ Html.Attributes.style [ ( "width", boardWidthString ++ "px" ), ( "display", "flex" ), ( "justify-content", "center" ), ( "font-size", (boardWidth / 32 |> toString) ++ "px" ) ] ]
                    [ model.gameState
                        |> gameStateToString
                        |> Html.text
                    ]
                , svg
                    [ width boardWidthString
                    , height boardHeightString
                    , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                    ]
                    [ renderBoard model.selected model.board
                    ]
                ]
            ]
            |> Html.App.map U
        ]


gameStateToString : GameState -> String
gameStateToString gameState =
    case gameState of
        Win ->
            "You won!"

        Loss ->
            "You lost!"

        _ ->
            ""


renderBoard : Maybe Piece -> Board -> Svg Msg
renderBoard selected board =
    let
        spaces =
            renderSpaces selected board
    in
        (Svg.path
            [ d
                <| ("M " ++ threeFifthsBoardWidthString ++ " 0 ")
                ++ ("L " ++ boardWidthString ++ " " ++ twoFifthsBoardHeightString)
                ++ ("L " ++ twoFifthsBoardWidthString ++ " " ++ boardHeightString)
                ++ ("L 0 " ++ threeFifthsBoardHeightString)
                ++ "Z"
            ]
            []
        )
            :: spaces
            |> g []


renderSpaces selected board =
    Model.boardIdPossibilities
        |> List.map
            (\boardId ->
                renderSpace selected (Model.getSpace boardId board) (getSpaceCoords boardId)
            )


renderSpace : Maybe Piece -> Space -> ( Float, Float ) -> Svg Msg
renderSpace selected space ( x, y ) =
    case space of
        EmptySpace ->
            Svg.text ""

        Space piece ->
            PieceView.renderPiece piece x y


getSpaceCoords boardId =
    case boardId of
        ZeroZero ->
            ( centerX, centerY )

        OneZero ->
            ( centerX, centerY )

        TwoZero ->
            ( centerX, centerY )

        ThreeZero ->
            ( centerX, centerY )

        ZeroOne ->
            ( centerX, centerY )

        OneOne ->
            ( centerX, centerY )

        TwoOne ->
            ( centerX, centerY )

        ThreeOne ->
            ( centerX, centerY )

        ZeroTwo ->
            ( centerX, centerY )

        OneTwo ->
            ( centerX, centerY )

        TwoTwo ->
            ( centerX, centerY )

        ThreeTwo ->
            ( centerX, centerY )

        ZeroThree ->
            ( centerX, centerY )

        OneThree ->
            ( centerX, centerY )

        TwoThree ->
            ( centerX, centerY )

        ThreeThree ->
            ( centerX, centerY )


boardWidth =
    720


boardWidthString =
    toString boardWidth


boardHeight =
    720


boardHeightString =
    toString boardHeight


centerX =
    boardWidth / 2


centerXString =
    toString centerX


centerY =
    boardHeight / 2


centerYString =
    toString centerY


twoFifthsBoardWidthString =
    toString (boardWidth * 2 / 5)


threeFifthsBoardWidthString =
    toString (boardWidth * 3 / 5)


twoFifthsBoardHeightString =
    toString (boardHeight * 2 / 5)


threeFifthsBoardHeightString =
    toString (boardHeight * 3 / 5)
