module Commands exposing (..)

import Window
import Task
import Messages exposing (..)


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Task.perform WindowSize Window.size
