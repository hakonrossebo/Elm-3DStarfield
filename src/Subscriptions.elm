module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseMove, onResize)
import Messages exposing (..)
import Models exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize OnWindowResize
        ]
