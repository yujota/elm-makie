module Makie.Types.Annotation exposing
    ( Annotation
    , Category
    , CategoryId
    , Handle
    , HandleProperty(..)
    , PointHandleType(..)
    , PointProperty
    , Project
    , Property(..)
    , RenderOptions
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Makie.Types.Geometry as G
import Time exposing (Posix)


type alias Annotation =
    { category : CategoryId
    , boundingBox : G.PBoundingBox
    , property : Property
    , createdTime : Posix
    }


type alias CategoryId =
    String


type Property
    = Point PointProperty


type alias PointProperty =
    { point : G.PPoint }


type alias Handle =
    { category : CategoryId
    , property : HandleProperty
    }


type HandleProperty
    = PointHandle PointHandleType



-- | RectangleHandle RectangleHandleType
-- | PolygonHandle PolygonHandleType


type PointHandleType
    = PointHandleMove G.CPoint


type alias RenderOptions =
    { pointRadius : Float
    , selectedPointRadius : Float
    , borderWidth : Float
    , touchTolerance : Float
    }


type alias Category =
    { name : String
    , color : Color
    , selectedColor : Color
    , fillColor : Color
    , selectedFillColor : Color
    , borderColor : Color
    , selectedBorderColor : Color
    }


type alias Project =
    { name : String
    , selectedCategory : Maybe CategoryId
    , categories : Dict CategoryId Category
    }
