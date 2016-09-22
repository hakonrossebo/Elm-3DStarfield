module WebGLView exposing (..)

import Html exposing (Html, Attribute, div, text, input)
import Color exposing (..)
import Color.Convert exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Models exposing (..)
import Messages exposing (..)

import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render
import Game.TwoD as Game


view : Model -> Html Msg
view model =
   let
        windowHeight =
            toString model.windowDimensions.height

        windowWidth =
            toString model.windowDimensions.width
    in
        Game.renderCentered { time = 0, camera = Camera.init ( 0, 3.5 ) 800, size = ( model.windowDimensions.width, model.windowDimensions.height ) }
                (List.append (drawStars model) (drawBackground model) )
        -- svg [ viewBox ("0 0 " ++ windowWidth ++ " " ++ windowHeight) ]
        --       [ background model windowWidth windowHeight
        --       , g [] (drawStars model)
        --       , infoText model windowWidth windowHeight
        --       ]


drawBackground : Model -> List Render.Renderable
drawBackground model =
   let
        windowHeight =
            toString model.windowDimensions.height

        windowWidth =
            toString model.windowDimensions.width

        windowCenterHeight =
            toFloat model.windowDimensions.height / 2

        windowCenterWidth =
            toFloat model.windowDimensions.width / 2

    in
        [Render.rectangle { color = Color.black, position = ( -400, -200 ), size = ( 800, 400 )}]

drawStars : Model -> List Render.Renderable
drawStars model =
   let
        windowHeight =
            toString model.windowDimensions.height

        windowWidth =
            toString model.windowDimensions.width

        windowCenterHeight =
            toFloat model.windowDimensions.height / 2

        windowCenterWidth =
            toFloat model.windowDimensions.width / 2

    in
        model.stars
        |>  List.sortBy (\star -> star.z)
        |>  List.reverse
        |>  List.map calculate2DPoint
        |>  List.map (drawStar windowCenterWidth windowCenterHeight)


drawStar : Float -> Float -> (Float, Float, Float) -> Render.Renderable
drawStar centerX centerY (x, y, z) =
  let
    x' = toString (x + centerX)
    y' = toString (y + centerY)
    size = (1.2 - z / bounds.maxDepth) * 4
    shade = round ((1- z/ bounds.maxDepth) * 255)
    shadeColor = rgb shade shade shade
  in
    Render.rectangle { color = shadeColor, position = ( x, y ), size = ( 1.8, 1.8 )}
    -- circle [ cx x' , cy y', r (toString size), fill (colorToHex shadeColor) ] []



calculate2DPoint : Star -> (Float, Float, Float)
calculate2DPoint {x, y, z} =
  let
    k = perspective / z
    newX = x * k
    newY = y * k
  in
    (newX, newY, z)




infoText : Model -> String -> String -> Svg.Svg Msg
infoText model windowWidth windowHeight=
    Svg.text' [x "10", y "20", fill "#ffffff", fontSize "22", fontFamily "monospace"][ Svg.text ("Fps: " ++ (model.fps |> toString))]


background : Model -> String -> String -> Svg.Svg Msg
background model windowWidth windowHeight=
    Svg.rect
        [ width <| windowWidth
        , height <| windowHeight
        , fill "black"
        ]
        []
