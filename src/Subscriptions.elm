module Subscriptions exposing (..)

import AnimationFrame
import Window
import Models exposing (..)
import Messages exposing (..)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes WindowSize
        ]



