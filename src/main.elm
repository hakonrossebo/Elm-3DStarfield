module Main exposing (initModelAndCommands, main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onMouseMove, onResize, onVisibilityChange)
import Color exposing (..)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes
import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


type Msg
    = NoOp
    | WindowSize Dom.Viewport
    | OnError String
    | Tick Float
    | OnWindowResize Int Int
    | OnVisibilityChange Visibility


type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    , minDepth : Float
    , maxDepth : Float
    }


bounds : Bounds
bounds =
    { minX = 45
    , minY = 40
    , maxX = 59
    , maxY = 59
    , minDepth = 1
    , maxDepth = 40
    }


type alias Star =
    { x : Float
    , y : Float
    , z : Float
    , px : Int
    , py : Int
    , color : Color
    }


defaultStar : Star
defaultStar =
    { x = -25, y = -25, z = bounds.maxDepth, px = 0, py = 0, color = Color.white }


type alias Model =
    { stars : List Star
    , seed : Seed
    , windowDimensions : { width : Float, height : Float }
    , fps : Int
    , zVelocity : Float
    , error : Maybe String
    , visibility : Visibility
    }


defaultModel : Model
defaultModel =
    let
        ( newStars, newSeed ) =
            addStars True (initialSeed 1234) []
    in
    { stars = newStars
    , seed = newSeed
    , windowDimensions = { width = 640, height = 480 }
    , fps = 0
    , zVelocity = 10
    , error = Nothing
    , visibility = Visible
    }


newStar : ( Float, Float ) -> Float -> Star
newStar ( newX, newY ) newZ =
    { defaultStar | x = newX, y = newY, z = newZ }


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Dom.getViewport
        |> Task.perform WindowSize


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = initModelAndCommands
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModelAndCommands : Flags -> ( Model, Cmd Msg )
initModelAndCommands flags =
    ( defaultModel, getWindowSizeCommand )


perspective : Float
perspective =
    256


starCount : Int
starCount =
    500


addStars : Bool -> Seed -> List Star -> ( List Star, Seed )
addStars initAllStars seed stars =
    if (starCount - List.length stars) <= 0 then
        ( stars, seed )

    else
        let
            ( star, newSeed ) =
                generateStar initAllStars bounds.minX bounds.minY bounds.maxDepth seed
        in
        addStars initAllStars newSeed (star :: stars)


generateStar : Bool -> Float -> Float -> Float -> Seed -> ( Star, Seed )
generateStar initAllStars minX minY maxZ seed =
    case initAllStars of
        True ->
            let
                newz =
                    Random.float (bounds.minDepth + 1) bounds.maxDepth

                ( randomz, newSeed ) =
                    Random.step newz seed

                pair =
                    Random.pair (Random.float -minX minX) (Random.float -minY minY)

                ( coords, newSeed2 ) =
                    Random.step pair newSeed
            in
            ( newStar coords randomz, newSeed2 )

        False ->
            let
                pair =
                    Random.pair (Random.float -minX minX) (Random.float -minY minY)

                ( coords, newSeed2 ) =
                    Random.step pair seed
            in
            ( newStar coords bounds.maxDepth, newSeed2 )


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


view : Model -> Html Msg
view model =
    let
        windowHeight =
            String.fromFloat model.windowDimensions.height

        windowWidth =
            String.fromFloat model.windowDimensions.width
    in
    svg [ viewBox ("0 0 " ++ windowWidth ++ " " ++ windowHeight) ]
        [ g [] (drawStars model)
        , infoText model windowWidth windowHeight
        ]


drawStars : Model -> List (Svg b)
drawStars model =
    let
        windowHeight =
            String.fromFloat model.windowDimensions.height

        windowWidth =
            String.fromFloat model.windowDimensions.width

        windowCenterHeight =
            model.windowDimensions.height / 2

        windowCenterWidth =
            model.windowDimensions.width / 2
    in
    model.stars
        |> List.sortBy (\star -> star.z)
        |> List.reverse
        |> List.map calculate2DPoint
        |> List.map (drawStar windowCenterWidth windowCenterHeight)


drawStar : Float -> Float -> ( Float, Float, Float ) -> Svg.Svg g
drawStar centerX centerY ( x, y, z ) =
    let
        x_ =
            String.fromFloat (x + centerX)

        y_ =
            String.fromFloat (y + centerY)

        size =
            (1.2 - z / bounds.maxDepth) * 4

        shade =
            1 - z / bounds.maxDepth

        shadeColor =
            rgb shade shade shade
    in
    circle [ cx x_, cy y_, r (String.fromFloat size), fill (Color.toCssString shadeColor) ] []


calculate2DPoint : Star -> ( Float, Float, Float )
calculate2DPoint { x, y, z } =
    let
        k =
            perspective / z

        newX =
            x * k

        newY =
            y * k
    in
    ( newX, newY, z )


infoText : Model -> String -> String -> Svg.Svg Msg
infoText model windowWidth windowHeight =
    Svg.text_ [ x "10", y "20", fill "#ffffff", fontSize "22", fontFamily "monospace" ] [ Svg.text ("Fps: " ++ (model.fps |> String.fromInt)) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize OnWindowResize
        , onVisibilityChange OnVisibilityChange
        ]
