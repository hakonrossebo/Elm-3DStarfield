module Main exposing (initModelAndCommands, main)

import Browser
import Commands exposing (..)
import Html
import Messages exposing (..)
import Models exposing (..)
import Subscriptions exposing (..)
import Updates exposing (..)
import View exposing (..)


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = initModelAndCommands
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


initModelAndCommands : Flags -> ( Model, Cmd Msg )
initModelAndCommands flags =
    ( defaultModel, getWindowSizeCommand )
