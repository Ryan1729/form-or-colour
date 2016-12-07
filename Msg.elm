module Msg exposing (..)

import Model exposing (..)


type Msg
    = NewGame Colouring
    | Place BoardId
    | Select Piece
    | SetWidth Float
    | Flip BoardId
