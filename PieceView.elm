module PieceView exposing (..)

import Svg exposing (Svg, svg, rect, path, circle, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Model exposing (..)
import Msg exposing (Msg(..))


rackWidth =
    250


rackHeight =
    600


rackWidthString =
    toString rackWidth


rackHeightString =
    toString rackHeight


halfRackWidth =
    rackWidth / 2


renderRack : Maybe Piece -> Rack -> Svg Msg
renderRack selected rack =
    svg
        [ width rackWidthString
        , height rackHeightString
        , viewBox ("0 0 " ++ rackWidthString ++ " " ++ rackHeightString)
        ]
    <|
        [ Svg.rect
            [ x "0"
            , y "0"
            , width rackWidthString
            , height rackHeightString
            , stroke "black"
            , strokeWidth "2"
            , fillOpacity "0"
            ]
            []
        ]
            ++ renderPieces selected rack


renderPieces : Maybe Piece -> Rack -> List (Svg Msg)
renderPieces selected rack =
    let
        ( plainTarget, colouredTarget ) =
            case selected of
                Nothing ->
                    ( Nothing, Nothing )

                Just (Piece colouring symbol) ->
                    case colouring of
                        Plain ->
                            ( Just ( toFloat rack.plain, symbol ), Nothing )

                        Coloured ->
                            ( Nothing, Just ( toFloat rack.coloured, symbol ) )
    in
        (List.range 1 rack.plain
            |> List.map (toFloat >> renderRackPiece plainTarget Plain 0)
        )
            ++ (List.range 1 rack.coloured
                    |> List.map (toFloat >> renderRackPiece colouredTarget Coloured halfRackWidth)
               )


renderRackPiece : Maybe ( Float, Symbol ) -> Colouring -> Float -> Float -> Svg Msg
renderRackPiece selected colouring x index =
    let
        y =
            rackHeight * (index - 0.5) / 8

        leftPieceX =
            halfPieceWidth + spacing

        oX =
            x + leftPieceX

        xX =
            x + leftPieceX * 3

        selectBox =
            case selected of
                Nothing ->
                    []

                Just ( target, symbol ) ->
                    if target == index then
                        [ renderSelectBox
                            (if symbol == O then
                                oX
                             else
                                xX
                            )
                            y
                        ]
                    else
                        []
    in
        g []
            ([ renderPiece (Piece colouring O) (oX) y
             , renderPiece (Piece colouring X) (xX) y
             ]
                ++ selectBox
            )


selectionOffset =
    halfPieceWidth + (spacing / 2)


selectionWidth =
    pieceWidth + spacing


selectionWidthString =
    toString selectionWidth


renderSelectBox : Float -> Float -> Svg Msg
renderSelectBox xPos yPos =
    rect
        [ x (toString (xPos - selectionOffset))
        , y (toString (yPos - selectionOffset))
        , strokeWidth "4"
        , stroke "#FFDC00"
        , fill "transparent"
        , width selectionWidthString
        , height selectionWidthString
        ]
        []


spacing =
    10


pieceWidth =
    (rackWidth / 4) - (2 * spacing)


pieceWidthString =
    toString pieceWidth


halfPieceWidth =
    pieceWidth / 2


halfPieceWidthString =
    toString halfPieceWidth


symbolRadius =
    halfPieceWidth - spacing


symbolRadiusString =
    toString symbolRadius


minusSymbolRadiusString =
    toString -symbolRadius


symbolDiameterString =
    toString (symbolRadius * 2)


minusSymbolDiameterString =
    toString (-symbolRadius * 2)


lightColour =
    "#FF4136"


darkColour =
    "#111111"


colourFromPiece piece =
    case piece of
        Piece Plain X ->
            darkColour

        Piece Plain O ->
            lightColour

        Piece Coloured X ->
            lightColour

        Piece Coloured O ->
            darkColour


renderPiece : Piece -> Float -> Float -> Svg Msg
renderPiece ((Piece colouring symbol) as piece) x y =
    let
        symbolColour =
            colourFromPiece piece

        extraAttributes =
            case colouring of
                Plain ->
                    [ fill "#EEEEEE" ]

                Coloured ->
                    [ fill "#0074D9" ]
    in
        g [] <|
            circle
                (extraAttributes
                    ++ [ cx (toString x)
                       , cy (toString y)
                       , r halfPieceWidthString
                       , strokeWidth "4"
                       , stroke symbolColour
                       ]
                )
                []
                :: renderSymbol symbolColour symbol x y


renderSymbol colour symbol x y =
    case symbol of
        X ->
            [ Svg.path
                [ d <|
                    ("M " ++ toString (x - symbolRadius) ++ " " ++ toString (y - symbolRadius))
                        ++ (" l " ++ symbolDiameterString ++ " " ++ symbolDiameterString)
                        ++ (" m " ++ minusSymbolDiameterString ++ " 0")
                        ++ (" l " ++ symbolDiameterString ++ " " ++ minusSymbolDiameterString)
                , stroke colour
                , strokeWidth "4"
                ]
                []
            ]

        O ->
            [ circle
                [ cx (toString x)
                , cy (toString y)
                , r symbolRadiusString
                , fill "transparent"
                , stroke colour
                , strokeWidth "4"
                ]
                []
            ]
