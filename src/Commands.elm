module Commands exposing (getWindowSizeCommand)

import Browser.Dom as Dom
import Messages exposing (..)
import Task


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Dom.getViewport
        |> Task.perform WindowSize



-- getWindowSizeCommand : Cmd Msg
-- getWindowSizeCommand =
--     let
--         processElement e =
--             case e of
--                 Ok result ->
--                     WindowSize result
--                 Err error ->
--                     case error of
--                         Dom.NotFound errorInfo ->
--                             OnError errorInfo
--     in
--     Dom.getElement "drawing-area"
--         |> Task.attempt processElement
