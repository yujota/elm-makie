module Makie.Types.Camera exposing (Camera, Constraint)

import Angle exposing (Angle)
import Makie.Types.Geometry as G
import Time exposing (Posix)


type alias Constraint =
    { lv0Width : G.PathologyPixels
    , lv0Height : G.PathologyPixels
    , minReductionRate : G.ReductionRate
    , maxReductionRate : G.ReductionRate
    }


type alias Camera =
    { center : G.PPoint
    , pHalfWidth : G.PathologyPixels
    , pHalfHeight : G.PathologyPixels
    , frame : G.CFrame
    , width : G.CanvasPixels
    , height : G.CanvasPixels
    , reductionRate : G.ReductionRate
    , lastUpdated : Posix
    , constraint : Constraint
    , angle : Angle
    }
