module Models exposing (..)

import Color exposing (..)
import Random exposing (..)
import Window

type alias Velocity =
    {
      x : Float
    , y : Float
    , z : Float
    }

velocity: Velocity
velocity =
    {
      x = 0.00
    , y = 0
    , z = 10
    }


perspective : Float
perspective = 256



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
    { minX = 45
    , minY = 40
    , maxX = 59
    , maxY = 59
    , minDepth = 1
    , maxDepth = 40
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
      (newStars, newSeed) = addStars True (initialSeed 1234) []
    in
    { stars = newStars
    , seed = newSeed
    , windowDimensions = { width = 640, height = 480 }
    , fps = 0
    }

newStar : (Float, Float) -> Float  -> Star
newStar (newX, newY) newZ =
    { defaultStar | x = newX, y = newY, z = newZ }

addStars : Bool -> Seed -> List Star -> (List Star, Seed)
addStars initAllStars seed stars =
    if (starCount - List.length stars) == 0 then
        (stars, seed)
    else
        let
            (star, newSeed) = (generateStar initAllStars bounds.minX bounds.minY bounds.maxDepth seed)
        in
            addStars initAllStars newSeed (star :: stars)



generateStar : Bool -> Float -> Float -> Float -> Seed -> (Star, Seed)
generateStar initAllStars minX minY maxZ seed =
        case initAllStars of
          True ->
            let
              newz = Random.float (bounds.minDepth + 1) bounds.maxDepth
              (randomz, newSeed) = Random.step newz seed
              pair = Random.pair (Random.float -minX minX) (Random.float -minY minY)
              (coords, newSeed2) = Random.step pair newSeed
            in
              (newStar coords randomz, newSeed2)
          False ->
            let
              yShift = velocity.y * 10
              lowY = -minX + yShift
              upperY = minY + yShift
              xShift = velocity.x * 10
              lowX = -minX + xShift
              upperX = minX + xShift
              pair = Random.pair (Random.float lowX upperX) (Random.float lowY upperY)
              (coords, newSeed2) = Random.step pair seed
            in
              (newStar coords bounds.maxDepth, newSeed2)

