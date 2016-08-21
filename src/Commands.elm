module Commands exposing (..)

import Window
import Task
import Messages exposing (..)

getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Task.perform (always NoOp) WindowSize Window.size
