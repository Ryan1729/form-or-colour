module ShapeOrColour exposing (..)

import Html exposing (program)
import Model exposing (Model, defaultModel)
import View exposing (view)
import Msg exposing (Msg)
import Update exposing (update)
import Ports
import Window
import Task


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Task.perform extractWidth Window.size )


extractWidth : Window.Size -> Msg
extractWidth =
    .width >> toFloat >> Msg.SetWidth


subscriptions : Model -> Sub Msg
subscriptions =
    [ Msg.NewGame
        |> always
        |> Ports.newGame
    , Window.resizes extractWidth
    ]
        |> Sub.batch
        |> always


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
