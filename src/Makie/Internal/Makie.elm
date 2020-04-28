module Makie.Internal.Makie exposing
    ( Action(..)
    , Annotation(..)
    , AnnotationHandle(..)
    , AnnotationHandleRecord
    , AnnotationRecord
    , Camera(..)
    , CameraRecord
    , Categories
    , Category(..)
    , CustomLabelRecord
    , Display
    , Event(..)
    , Gesture(..)
    , GestureHandlingStatus(..)
    , GestureModel
    , IdGenerator(..)
    , Image(..)
    , ImageBoundingBox
    , ImagePixels
    , ImagePoint
    , ImagePolygon
    , ImageRectangle
    , ImageSystem
    , ImageVector
    , Label(..)
    , LabelColors
    , Makie(..)
    , MakieRecord
    , Mode(..)
    , Notices
    , ObjectContainer
    , PanePoint
    , PaneSystem
    , PaneVector
    , PointShape
    , PointerEvent(..)
    , PolygonShape
    , RectangleShape
    , ReductionRate
    , RenderRequest(..)
    , Shape(..)
    , Target(..)
    , WheelEvent(..)
    , defaultNotices
    , fromPanePoint
    , fromPaneVector
    , imagePixels
    , imagePoint
    , imageVector
    , inImagePixels
    , inReductionRate
    , panePoint
    , paneVector
    , reductionRate
    , toPanePoint
    , toPaneVector
    )

import Angle exposing (Angle)
import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Color exposing (Color)
import Dict exposing (Dict)
import Frame2d exposing (Frame2d)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate(..))
import Random.Pcg.Extended
import Rectangle2d exposing (Rectangle2d)
import Set exposing (Set)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type Makie
    = Makie MakieRecord


type alias MakieRecord =
    { camera : CameraRecord
    , target : Target
    , mode : Mode
    , paneWidth : Int
    , paneHeight : Int
    , imageWidth : Int
    , imageHeight : Int
    , annotations : ObjectContainer AnnotationRecord
    , gestureModel : GestureModel
    , defaultLabel : Maybe Label
    , categories : Categories
    , display : Display
    , renderedTime : Posix
    , idGenerator : IdGenerator
    }


type IdGenerator
    = Incremental { category : Int, annotation : Int }
    | UseUuid Random.Pcg.Extended.Seed


type Image
    = Image { src : String, name : String, width : Int, height : Int }


type Mode
    = BrowseMode
    | PointMode
    | RectangleMode
    | PolygonMode
    | SelectionRectangleMode


type Target
    = NoTarget
    | TargetSelected String AnnotationRecord -- Key, Annotation
    | TargetCreating AnnotationHandleRecord
    | TargetEditing String AnnotationHandleRecord



-- Events


type Event
    = PointerEventVariant PointerEvent
    | WheelEventVariant WheelEvent
    | RefreshPane Posix
    | SingleImageCanvasTextureLoaded (Maybe Texture)
    | OpenLabelEdit String
    | SetMode Mode


type PointerEvent
    = OnDown Pointer.Event
    | OnMove Pointer.Event
    | OnUp Pointer.Event
    | OnCancel Pointer.Event
    | OnOut Pointer.Event
    | OnLeave Pointer.Event


type WheelEvent
    = OnWheel Wheel.Event



-- Gestures


type alias GestureModel =
    { status : GestureHandlingStatus
    , isSpaceKeyPressed : Bool
    , isPenDeviceDetected : Bool
    }


type Gesture
    = NoGesture
    | MouseStart { shiftKey : Bool, spaceKey : Bool } PanePoint
    | MouseOnGoing { shiftKey : Bool, spaceKey : Bool } PanePoint PanePoint -- options, last point, current point
    | MouseEnd { shiftKey : Bool, spaceKey : Bool } PanePoint PanePoint
    | PenStart PanePoint
    | PenOnGoing PanePoint PanePoint
    | PenEnd PanePoint PanePoint
    | SingleTouchStart { smartStylus : Bool } PanePoint
    | SingleTouchOnGoing { smartStylus : Bool } PanePoint PanePoint
    | SingleTouchEnd { smartStylus : Bool } PanePoint PanePoint


type GestureHandlingStatus
    = HandlingNothing
    | Cooling (Set Int)
      {- Cooling is used not to invoke gestures
         when multi-pointer gesture is finished but not all of pointers are released.
      -}
    | HandlingMouse { pointerId : Int, lastPosition : PanePoint, shiftKey : Bool, spaceKey : Bool }
    | HandlingPen { pointerId : Int, lastPosition : PanePoint }
    | HandlingSingleTouch { pointerId : Int, lastPosition : PanePoint, smartStylus : Bool }



-- Actions


type Action
    = NoAction
    | Batch (List Action)
    | Move PaneVector
    | Zoom PanePoint ReductionRate
    | Rotate PanePoint Angle
    | Add AnnotationRecord
    | Insert String AnnotationRecord
    | Delete String



-- Camera


type Camera
    = Camera CameraRecord


type alias CameraRecord =
    { imageFrame : ImageFrame
    , reductionRate : ReductionRate
    , angle : Angle
    }



-- Annotation


{-| internal 以外では使わない
-}
type Annotation
    = Annotation AnnotationRecord


type alias AnnotationRecord =
    { label : Maybe Label, notices : Notices, shape : Shape }


type alias AnnotationHandleRecord =
    { label : Maybe Label, notices : Notices, handle : AnnotationHandle }


type alias Notices =
    { shape : Maybe Notice, label : Maybe Notice }


defaultNotices =
    { shape = Nothing, label = Nothing }


type Shape
    = Point PointShape
    | Rectangle RectangleShape
    | Polygon PolygonShape


type alias PointShape =
    { point : ImagePoint }


type alias RectangleShape =
    { rectangle : ImageRectangle }


type alias PolygonShape =
    { polygon : ImagePolygon }


type Notice
    = Warning String
    | Danger String


type AnnotationHandle
    = PointMove PointShape { start : ImagePoint, control : ImagePoint }
    | RectangleMove RectangleShape { start : ImagePoint, control : ImagePoint }
    | RectangleEditCorner { oppositeCorner : ImagePoint, control : ImagePoint }


type alias AnnotationOptions =
    { point : PointAnnotationOptions }


type alias PointAnnotationOptions =
    { radius : Int }



-- Labels


type Label
    = BelongsToCategory String
    | CustomLabel CustomLabelRecord


type alias CustomLabelRecord =
    { name : String
    , colors : LabelColors
    }


type alias LabelColors =
    { lineColor : Color
    , selectedLineColor : Color
    , fillColor : Color
    , selectedFillColor : Color
    }


type alias Categories =
    {}


type Category
    = CategoryRoot (List Category)
    | CategoryNode String String Color (List Category)
    | CategoryLeaf String String Color



-- View
-- Canvas


type alias Display =
    { texture : Maybe Texture
    , src : String
    , images : List Renderable
    , annotations : List Renderable
    , editing : List Renderable
    , request : RenderRequest
    }


type RenderRequest
    = NotNecessary
    | RenderImages
    | RenderEditing
    | RenderAnnotations
    | RenderAll



-- Geometry
-- PathologySystem


type ImageSystem
    = ImageSystem


type ImageSystemPixels
    = PathologySystemPixels


type alias ImagePixels =
    Quantity Float ImageSystemPixels


type alias ImagePoint =
    Point2d ImageSystemPixels ImageSystem


type alias ImageRectangle =
    Rectangle2d ImageSystemPixels ImageSystem


type alias ImagePolygon =
    Polygon2d ImageSystemPixels ImageSystem


type alias ImageVector =
    Vector2d ImageSystemPixels ImageSystem


type alias ImageBoundingBox =
    BoundingBox2d ImageSystemPixels ImageSystem


type alias ImageFrame =
    Frame2d Pixels PaneSystem { defines : ImageSystem }



-- Pane System


type PaneSystem
    = PaneSystem


type alias PanePixels =
    Quantity Float Pixels


type alias PanePoint =
    Point2d Pixels PaneSystem


type alias PaneVector =
    Vector2d Pixels PaneSystem



-- Rate


type alias ReductionRateUnit =
    Rate ImageSystemPixels Pixels


type alias ReductionRate =
    Quantity Float ReductionRateUnit



-- Helper functions for geometric types


imagePixels : number -> Quantity number ImageSystemPixels
imagePixels n =
    Quantity n


inImagePixels : Quantity number ImageSystemPixels -> number
inImagePixels (Quantity n) =
    n


imagePoint : { x : Float, y : Float } -> ImagePoint
imagePoint r =
    Point2d.xy (imagePixels r.x) (imagePixels r.y)


imageVector : { dx : Float, dy : Float } -> ImageVector
imageVector r =
    Vector2d.xy (imagePixels r.dx) (imagePixels r.dy)


panePoint : { x : Float, y : Float } -> PanePoint
panePoint r =
    Point2d.pixels r.x r.y


paneVector : { dx : Float, dy : Float } -> PaneVector
paneVector r =
    Vector2d.pixels r.dx r.dy


toPanePoint : ReductionRate -> ImageFrame -> ImagePoint -> PanePoint
toPanePoint rRate iFrame iPoint =
    -- TODO: Need to be tested
    iPoint |> Point2d.at_ rRate |> Point2d.placeIn iFrame


fromPanePoint : ReductionRate -> ImageFrame -> PanePoint -> ImagePoint
fromPanePoint rRate iFrame pPoint =
    -- TODO: Need to be tested
    pPoint |> Point2d.relativeTo iFrame |> Point2d.at rRate


toPaneVector : ReductionRate -> ImageFrame -> ImageVector -> PaneVector
toPaneVector rRate pFrame iVector =
    -- TODO: Need to be tested
    iVector |> Vector2d.at_ rRate |> Vector2d.placeIn pFrame


fromPaneVector : ReductionRate -> ImageFrame -> PaneVector -> ImageVector
fromPaneVector rRate pFrame pVector =
    -- TODO: Need to be tested
    pVector |> Vector2d.relativeTo pFrame |> Vector2d.at rRate


reductionRate : Float -> Quantity Float ReductionRateUnit
reductionRate n =
    Quantity n


inReductionRate : Quantity Float ReductionRateUnit -> Float
inReductionRate (Quantity n) =
    n



-- Helpers


type alias ObjectContainer o =
    { linerQuaternaryTree : Array (Set String) -- Hashtable for (Morton Order -> Annotation Id)
    , objects : Dict String { index : Int, object : o, boundingBox : ImageBoundingBox }
    , depth : Int
    , unitSize : Int
    }
