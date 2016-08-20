
import Html exposing (Html, Attribute, div, text, input)
import Html.App exposing (program)
import Color exposing (..)
import Color.Convert exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)
import AnimationFrame
import Random exposing (..)
import Window
import Task

main : Program Never
main =
    program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-------- Init -------------------------
initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, getWindowSizeCommand )


-------- Commands -------------------------
getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Task.perform (always NoOp) WindowSize Window.size


-------- Subscriptions -------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes WindowSize
        ]


-------- Model -------------------------
velocity : Float
velocity = 0.006

perspective : Float
perspective = 512



starCount : Int
starCount = 500

type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    , minDepth : Float
    , maxDepth : Float
    }

bounds: Bounds
bounds =
    { minX = 30
    , minY = 20
    , maxX = 300
    , maxY = 300
    , minDepth = 0
    , maxDepth = 32
    }

type alias Star =
    { x: Float
    , y: Float
    , z: Float
    , px: Int
    , py: Int
    , color: Color
    }


defaultStar : Star
defaultStar = { x = -25, y = -25, z = bounds.maxDepth, px = 0, py = 0, color = Color.white}

type alias Model =
    { stars : List Star
    , seed : Seed
    , windowDimensions : Window.Size
    , fps : Int
    }


defaultModel : Model
defaultModel =
    let
      (newStars, newSeed) = addStars True []  (initialSeed 1234)
    in
    { stars = newStars
    , seed = newSeed
    , windowDimensions = { width = 640, height = 480 }
    , fps = 0
    }


-------- Messages -------------------------
type Msg
    = NoOp
    | WindowSize Window.Size
    | Tick Time.Time



-------- Update -------------------------
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
                    let
                        movedStars = List.map (moveStar (velocity * dt ) ) model.stars
                        insideField = List.filter filterInside movedStars
                        (updatedStars, updatedSeed) = addStars False insideField model.seed
                        model' =  { model |
                                stars = updatedStars,
                                seed = updatedSeed,
                                fps = 1000/dt |> round
                            }
                    in
                        (model', Cmd.none)
    in
        ( newModel, cmds )

newStar : (Float, Float) -> Float  -> Star
newStar (newX, newY) newZ =
    { defaultStar | x = newX, y = newY, z = newZ }

addStars : Bool -> List Star -> Seed -> (List Star, Seed)
addStars initAllStars stars seed =
    if (starCount - List.length stars) == 0 then
        (stars, seed)
    else
        let
            (star, newSeed) = (generateStar initAllStars bounds.minX bounds.minY bounds.maxDepth seed)
        in
            addStars initAllStars (star :: stars) newSeed



generateStar : Bool -> Float -> Float -> Float -> Seed -> (Star, Seed)
generateStar initAllStars minX minY maxZ seed =
        case initAllStars of
          True ->
            let
              newz = Random.float 0 bounds.maxDepth
              (randomz, newSeed) = Random.step newz seed
              pair = Random.pair (Random.float -minX minX) (Random.float -minY minY)
              (coords, newSeed2) = Random.step pair newSeed
            in
              (newStar coords randomz, newSeed2)
          False ->
            let
              pair = Random.pair (Random.float -minX minX) (Random.float -minY minY)
              (coords, newSeed2) = Random.step pair seed
            in
              (newStar coords bounds.maxDepth, newSeed2)


moveStar : Float -> Star -> Star
moveStar velocity star =
    { star |
        z = star.z - velocity
    }


filterInside : Star -> Bool
filterInside star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxX && star.z > bounds.minDepth


-------- View -------------------------
view : Model -> Html Msg
view model =
   let
        windowHeight =
            toString model.windowDimensions.height

        windowWidth =
            toString model.windowDimensions.width
    in
        svg [ viewBox ("0 0 " ++ windowWidth ++ " " ++ windowHeight) ]
              [ background model windowWidth windowHeight
              , g [] (drawStars model)
              , infoText model windowWidth windowHeight
              ]

drawStars : Model -> List (Svg b)
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
        |>  List.map calculate2DPoint
        |>  List.map (drawStar windowCenterWidth windowCenterHeight)


drawStar : Float -> Float -> (Float, Float, Float) -> Svg.Svg g
drawStar centerX centerY (x, y, z) =
  let
    x' = toString (x + centerX)
    y' = toString (y + centerY)
    size = (1.2 - z / bounds.maxDepth) * 4
    shade = round ((1- z/ bounds.maxDepth) * 255)
    shadeColor = rgb shade shade shade
  in
  circle [ cx x' , cy y', r (toString size), fill (colorToHex shadeColor) ] []



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
