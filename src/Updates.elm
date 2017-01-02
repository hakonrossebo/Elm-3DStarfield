module Updates exposing (..)

import Time exposing (..)
import Models exposing (..)
import Messages exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                WindowSize newSize ->
                    ( { model | windowDimensions = newSize }, Cmd.none )

                Tick dt ->
                    updateStep model dt
    in
        ( newModel, cmds )


updateStep : Model -> Time.Time -> ( Model, Cmd Msg )
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
