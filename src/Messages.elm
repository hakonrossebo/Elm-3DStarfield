module Messages exposing (Msg(..))

import Browser.Dom as Dom


type Msg
    = NoOp
    | WindowSize Dom.Viewport
    | OnError String
    | Tick Float
    | OnWindowResize Int Int
