module Messages exposing (..)

import Window exposing (..)
import Time exposing (..)

type Msg
    = NoOp
    | WindowSize Window.Size
    | Tick Time.Time
