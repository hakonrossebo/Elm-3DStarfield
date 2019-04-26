module Commands exposing (getWindowSizeCommand)

import Browser.Dom as Dom
import Messages exposing (..)
import Task


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Dom.getViewport
        |> Task.perform WindowSize
