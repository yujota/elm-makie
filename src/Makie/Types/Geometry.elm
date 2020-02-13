module Makie.Types.Geometry exposing
    ( CFrame
    , CPoint
    , CVector
    , CanvasPixels
    , CanvasSystem
    , PBoundingBox
    , PPoint
    , PVector
    , PathologyPixels
    , PathologySystem
    , ReductionRate
    , cPoint
    , inPathologyPixels
    , inReductionRate
    , pPoint
    , pathologyPixels
    , reductionRate
    )

import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate(..))
import Vector2d exposing (Vector2d)



-- Reduction Rate (= 1 / power)


type alias ReductionRateUnit =
    Rate LevelZeroPixels Pixels


type alias ReductionRate =
    Quantity Float ReductionRateUnit


reductionRate : Float -> Quantity Float ReductionRateUnit
reductionRate numReductionRate =
    Quantity numReductionRate


calcReductionRate : CanvasPixels -> PathologyPixels -> Quantity Float ReductionRateUnit
calcReductionRate cLength pLength =
    inPathologyPixels pLength / Pixels.inPixels cLength |> reductionRate


inReductionRate : Quantity Float ReductionRateUnit -> Float
inReductionRate (Quantity numReductionRate) =
    numReductionRate



-- Pathology System


type LevelZeroPixels
    = LevelZeroPixels


type alias PathologyPixels =
    Quantity Float LevelZeroPixels


type PathologySystem
    = PathologySystem


type alias PPoint =
    Point2d LevelZeroPixels PathologySystem


type alias PVector =
    Vector2d LevelZeroPixels PathologySystem


type alias PBoundingBox =
    BoundingBox2d LevelZeroPixels PathologySystem


type alias PFrame =
    Frame2d LevelZeroPixels PathologySystem {}


pathologyPixels : number -> Quantity number LevelZeroPixels
pathologyPixels numPixels =
    Quantity numPixels


inPathologyPixels : Quantity number LevelZeroPixels -> number
inPathologyPixels (Quantity numPixels) =
    numPixels


pPoint : Float -> Float -> Point2d LevelZeroPixels PathologySystem
pPoint x y =
    Point2d.xy (pathologyPixels x) (pathologyPixels y)



-- Canvas System


type alias CanvasPixels =
    Quantity Float Pixels


type CanvasSystem
    = CanvasSystem


type alias CPoint =
    Point2d Pixels CanvasSystem


type alias CVector =
    Vector2d Pixels CanvasSystem


type alias CFrame =
    Frame2d Pixels PathologySystem { defines : CanvasSystem }


cPoint : Float -> Float -> Point2d Pixels CanvasSystem
cPoint x y =
    Point2d.pixels x y
