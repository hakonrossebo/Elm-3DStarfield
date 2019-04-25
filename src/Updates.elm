module Updates exposing (filterVisibleStars, moveStar, update, updateStep)

import Commands exposing (..)
import Messages exposing (..)
import Models exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                WindowSize element ->
                    ( { model | windowDimensions = { width = element.viewport.width, height = element.viewport.height } }, Cmd.none )

                Tick dt ->
                    updateStep model dt

                -- OnSketchDrawingAreaFound element ->
                --     ( { model | sketchDrawingArea = Just element }, Cmd.none )
                OnError error ->
                    ( { model | error = Just error }, Cmd.none )

                OnWindowResize x y ->
                    ( model, getWindowSizeCommand )
    in
    ( newModel, cmds )


updateStep : Model -> Float -> ( Model, Cmd Msg )
updateStep model dt =
    let
        ( updatedStars, updatedSeed ) =
            model.stars
                |> List.map (moveStar velocity dt)
                |> List.filter filterVisibleStars
                |> addStars False model.seed
    in
    ( { model
        | stars = updatedStars
        , seed = updatedSeed
        , fps = 1000 / dt |> round
      }
    , Cmd.none
    )


moveStar : Velocity -> Float -> Star -> Star
moveStar velocity dt star =
    let
        xVelocity =
            (dt * velocity.x) / 1000

        yVelocity =
            (dt * velocity.y) / 1000

        zVelocity =
            (dt * velocity.z) / 1000
    in
    { star
        | x = star.x - xVelocity
        , y = star.y - yVelocity
        , z = star.z - zVelocity
    }


filterVisibleStars : Star -> Bool
filterVisibleStars star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxX && abs star.z > bounds.minDepth
