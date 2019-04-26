module Updates exposing (filterVisibleStars, moveStar, update, updateStep)

import Browser.Events exposing (Visibility(..))
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

                OnError error ->
                    ( { model | error = Just error }, Cmd.none )

                OnWindowResize x y ->
                    ( model, getWindowSizeCommand )

                OnVisibilityChange visibility ->
                    ( { model | visibility = visibility }, Cmd.none )
    in
    ( newModel, cmds )


updateStep : Model -> Float -> ( Model, Cmd Msg )
updateStep model dt =
    let
        ( updatedStars, updatedSeed ) =
            model.stars
                |> List.map (moveStar model.zVelocity dt)
                |> List.filter filterVisibleStars
                |> addStars False model.seed

        newModel =
            case model.visibility of
                Visible ->
                    { model
                        | stars = updatedStars
                        , seed = updatedSeed
                        , fps = 1000 / dt |> round
                    }

                Hidden ->
                    model
    in
    ( newModel
    , Cmd.none
    )


moveStar : Float -> Float -> Star -> Star
moveStar velocity dt star =
    let
        zVelocity =
            (dt * velocity) / 1000
    in
    { star
        | z = star.z - zVelocity
    }


filterVisibleStars : Star -> Bool
filterVisibleStars star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxX && abs star.z > bounds.minDepth
