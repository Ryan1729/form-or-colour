port module Ports exposing (..)


port newPlainGame : (() -> msg) -> Sub msg


port newColouredGame : (() -> msg) -> Sub msg


port alert : String -> Cmd msg
