module ShapeOrColour exposing (..)

import Html exposing (program)
import Model exposing (Model, defaultModel)
import View exposing (view)
import Msg exposing (Msg)
import Update exposing (update)
import Ports


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    Msg.NewGame
        |> always
        |> Ports.newGame
        |> always


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
