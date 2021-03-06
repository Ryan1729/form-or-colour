module View exposing (view)

import Model exposing (..)
import Html exposing (Html, text)
import Html.Attributes
import Msg exposing (Msg(..))
import Svg exposing (Svg, svg, rect, path, circle, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import PieceView


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "flex-direction"
              , if model.width < boardWidthPlusMargin then
                    "column"
                else
                    "row"
              )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "margin-top", "1vh" )
                ]
            ]
            [ PieceView.renderRack model.playerColouring model.selected model.rack ]
        , Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "text-align", "center" )
                , ( "flex-direction", "column" )
                , ( "margin", boardMarginString ++ "vw" )
                , ( "font", "6vw Roboto,sans-serif" )
                ]
            ]
            [ model.gameState
                |> gameStateToString
                |> Html.text
            , svg
                [ width boardWidthString
                , height boardHeightString
                , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                ]
                [ renderBoard model.playerColouring model.selected model.board
                ]
            ]
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


renderBoard : Colouring -> Maybe Piece -> Board -> Svg Msg
renderBoard playerColouring selected board =
    let
        spaces =
            renderSpaces playerColouring selected board
    in
        (Svg.path
            [ d <|
                ("M " ++ threeFifthsBoardWidthString ++ " 0 ")
                    ++ ("L " ++ boardWidthString ++ " " ++ twoFifthsBoardHeightString)
                    ++ ("L " ++ twoFifthsBoardWidthString ++ " " ++ boardHeightString)
                    ++ ("L 0 " ++ threeFifthsBoardHeightString)
                    ++ "Z"
            , fill
                "#FFDC00"
            ]
            []
        )
            :: spaces
            ++ boardText playerColouring
            |> g []


boardText : Colouring -> List (Svg Msg)
boardText playerColouring =
    let
        ( text1, text2 ) =
            case playerColouring of
                Plain ->
                    ( "Form", "Colour" )

                Coloured ->
                    ( "Colour", "Form" )

        x1 =
            toString (boardWidth * 1 / 4)

        y1 =
            toString (boardHeight * 3 / 4)

        x2 =
            toString (boardWidth * 3 / 4)

        y2 =
            toString (boardHeight * 1 / 4)
    in
        [ Svg.text_
            [ x x1
            , y y1
            , transform <| "rotate(45 " ++ x1 ++ "," ++ y1 ++ ")"
            , style <| "fill:transparent; stroke:#cf8c28;font-size:66px"
            , textAnchor "middle"
            ]
            [ Svg.text text1 ]
        , Svg.text_
            [ x x2
            , y y2
            , transform <| "rotate(225 " ++ x2 ++ "," ++ y2 ++ ")"
            , style <| "fill:transparent; stroke:#cf8c28;font-size:66px"
            , textAnchor "middle"
            ]
            [ Svg.text text2 ]
        ]


renderSpaces playerColouring selected board =
    Model.boardIdPossibilities
        |> List.map
            (\boardId ->
                renderSpace playerColouring selected boardId (Model.getSpace boardId board) (getSpaceCoords boardId)
            )


renderSpace : Colouring -> Maybe Piece -> BoardId -> Space -> ( Float, Float ) -> Svg Msg
renderSpace playerColouring selected boardId space ( x, y ) =
    let
        pieceView : Svg Msg
        pieceView =
            case space of
                EmptySpace ->
                    Svg.text ""

                Space ((Piece colouring _) as piece) ->
                    let
                        attributes =
                            if colouring == playerColouring then
                                [ onClick (Flip boardId) ]
                            else
                                []
                    in
                        PieceView.renderPiece attributes piece x y

        spaceAttriutes =
            case selected of
                Just _ ->
                    [ onClick (Place boardId) ]

                Nothing ->
                    []
    in
        [ circle
            ([ cx (toString x)
             , cy (toString y)
             , r <| toString (PieceView.halfPieceWidth + 15)
             , fill "#888888"
             , fillOpacity "0.25"
             , strokeWidth "4"
             , stroke "#888888"
             ]
                ++ spaceAttriutes
            )
            []
        , pieceView
        ]
            |> g []


getSpaceCoords boardId =
    case boardId of
        ZeroZero ->
            ( centerX, boardHeight * 1 / 5 )

        OneZero ->
            ( boardWidth * 3 / 5, boardHeight * 3 / 10 )

        TwoZero ->
            ( boardWidth * 7 / 10, boardHeight * 2 / 5 )

        ThreeZero ->
            ( boardWidth * 4 / 5, centerY )

        ZeroOne ->
            ( boardWidth * 2 / 5, boardHeight * 3 / 10 )

        OneOne ->
            ( centerX, boardHeight * 2 / 5 )

        TwoOne ->
            ( boardWidth * 3 / 5, centerY )

        ThreeOne ->
            ( boardWidth * 7 / 10, boardHeight * 3 / 5 )

        ZeroTwo ->
            ( boardWidth * 3 / 10, boardHeight * 2 / 5 )

        OneTwo ->
            ( boardWidth * 2 / 5, centerY )

        TwoTwo ->
            ( centerX, boardHeight * 3 / 5 )

        ThreeTwo ->
            ( boardWidth * 3 / 5, boardHeight * 7 / 10 )

        ZeroThree ->
            ( boardWidth / 5, centerY )

        OneThree ->
            ( boardWidth * 3 / 10, boardHeight * 3 / 5 )

        TwoThree ->
            ( boardWidth * 2 / 5, boardHeight * 7 / 10 )

        ThreeThree ->
            ( centerX, boardHeight * 4 / 5 )


boardWidth =
    700


boardWidthString =
    toString boardWidth


boardHeight =
    700


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


boardMargin =
    2


boardMarginString =
    toString boardMargin


boardWidthPlusMargin =
    boardWidth * (1 + boardMargin / 100)
