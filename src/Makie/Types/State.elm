module Makie.Types.State exposing
    ( AnnotationModeType(..)
    , AnnotationState
    , ImageState
    , Interaction
    , Mode(..)
    , Options
    , Params
    , State
    , TouchState(..)
    , WorkingAnnotation(..)
    )

import Angle exposing (Angle)
import Array exposing (Array)
import Canvas as C
import Canvas.Texture as Texture
import Dict exposing (Dict)
import Makie.Internal.Camera as Camera exposing (Camera)
import Makie.Internal.Pathology as Pathology exposing (Pathology)
import Makie.Types.Annotation as A
import Makie.Types.Data as D
import Makie.Types.Geometry as G
import Time exposing (Posix)


type Mode
    = BrowseMode
    | AnnotationMode AnnotationModeType


type AnnotationModeType
    = PointAnnotationMode
    | RectangleAnnotationMode
    | PolygonAnnotationMode


type alias State msg =
    { camera : Camera
    , width : Int
    , height : Int
    , currentTime : Posix
    , mode : Mode
    , cameraActions : List Camera.Action
    , image : ImageState msg
    , annotation : AnnotationState
    , interaction : Interaction
    , error : ErrorState
    }


type alias ImageState msg =
    { pathology : Pathology
    , toDraw : List C.Renderable
    , sources : List (Texture.Source msg)
    , pathologyStatus : Pathology.Status
    }


type alias AnnotationState =
    { replica : AnnotationReplica
    , toDraw : List C.Renderable
    , working : WorkingAnnotation
    , replicaStatus : String
    }


type alias AnnotationReplica =
    Dict String A.Annotation


type WorkingAnnotation
    = NotSelected
    | Selected { id : String, entity : A.Annotation }
    | Handled
        { id : String
        , handle : A.Handle
        , cachedEntity : A.Annotation
        }
    | NewHandle
        { handle : A.Handle
        }


type alias Interaction =
    { isDragging : Bool
    , touch : TouchState
    }


type TouchState
    = NoTouch
    | SingleTouch Int G.CPoint -- Touch Id
    | DoubleTouch { idA : Int, idB : Int, pointA : G.CPoint, pointB : G.CPoint }
    | ZoomTouch
        { idA : Int
        , idB : Int
        , pointA : G.CPoint
        , pointB : G.CPoint
        , initialLength : G.CanvasPixels
        , baseReductionRate : G.ReductionRate
        }
    | RotateAndZoomTouch
        { idA : Int
        , idB : Int
        , pointA : G.CPoint
        , pointB : G.CPoint
        , initialLength : G.CanvasPixels
        , initialAngle : Angle
        , baseReductionRate : G.ReductionRate
        , baseAngle : Angle
        , lastAngle : Angle
        }


type alias ErrorState =
    { errors : Array ( Posix, String )
    }


type alias Options =
    { render : A.RenderOptions }


type alias Params =
    { options : Options, project : A.Project, data : D.Data }
