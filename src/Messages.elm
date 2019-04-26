module Messages exposing (Msg(..))

import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))


type Msg
    = NoOp
    | WindowSize Dom.Viewport
    | OnError String
    | Tick Float
    | OnWindowResize Int Int
    | OnVisibilityChange Visibility
