module Main exposing (..)

import Html
import Models exposing (..)
import Subscriptions exposing (..)
import Messages exposing (..)
import Commands exposing (..)
import View exposing (..)
import Updates exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = View.view
        }


initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, getWindowSizeCommand )
