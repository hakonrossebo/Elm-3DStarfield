import Html.App exposing (program)

import Models exposing (..)
import Subscriptions exposing (..)
import Messages exposing (..)
import Commands exposing (..)
import WebGLView exposing (..)
import Updates exposing (..)

main : Program Never
main =
    program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = WebGLView.view
        }

initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, getWindowSizeCommand )
