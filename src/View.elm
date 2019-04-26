module View exposing (calculate2DPoint, drawStar, drawStars, infoText, view)

import Color exposing (..)
import Color.Convert exposing (..)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes
import Messages exposing (..)
import Models exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
    circle [ cx x_, cy y_, r (String.fromFloat size), fill (colorToHex shadeColor) ] []


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
