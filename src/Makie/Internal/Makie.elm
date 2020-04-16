module Makie.Internal.Makie exposing
    ( Action(..)
    , Camera(..)
    , CameraAction(..)
    , CameraRecord
    , Event(..)
    , EventMode(..)
    , EventStatus(..)
    , EventStatusRecord
    , Image(..)
    , Makie(..)
    , MakieRecord
    , PanePoint
    , PaneSystem
    , PaneVector
    , initialEventStatus
    )

import Angle exposing (Angle)
import Canvas.Texture exposing (Texture)
import Html.Events.Extra.Pointer as Pointer
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type Makie
    = Makie MakieRecord


type alias MakieRecord =
    { eventStatus : EventStatus
    , image : Image
    , paneWidth : Int
    , paneHeight : Int
    }


type Image
    = Image { src : String, name : String, width : Int, height : Int }



-- Events


type Event
    = PointerEvent PointerEventSpec
    | RefreshPane Posix
    | CanvasTextureLoaded Texture


type PointerEventSpec
    = OnDown Pointer.Event
    | OnMove Pointer.Event
    | OnUp Pointer.Event
    | OnCancel Pointer.Event
    | OnOut Pointer.Event
    | OnLeave Pointer.Event


type EventStatus
    = EventStatus EventStatusRecord


type alias EventStatusRecord =
    { mode : EventMode }


type EventMode
    = ZeroPointer
    | OnePointer Pointer.Event
    | TwoPointer { id1 : Int, id2 : Int, history1 : List ( Float, Float ), history2 : List ( Float, Float ) }
    | ZoomPointer
    | RotatePointer


initialEventStatus : EventStatus
initialEventStatus =
    EventStatus { mode = ZeroPointer }



-- Actions


type Action
    = CameraActionVariant CameraAction
    | AnnotationAction
    | NoAction



-- Camera


type Camera
    = Camera {}


type alias CameraRecord =
    { x : Float, y : Float }


type CameraAction
    = Move MoveSpec
    | Zoom ZoomPoint ZoomMagnification
    | Resize { width : PanePixels, height : PanePixels }
    | Rotate RotationPoint RotationSpec


type MoveSpec
    = MoveByPaneVector PaneVector


type ZoomPoint
    = ZoomByPanePoint PanePoint


type ZoomMagnification
    = ZoomRatio Float
    | ZoomTo Float


type RotationPoint
    = RotateByPanePoint PanePoint


type RotationSpec
    = RelativeAngle Angle
    | AbsoluteAngle Angle



-- Geometry


type alias PanePixels =
    Quantity Float Pixels


type PaneSystem
    = PaneSystem


type alias PanePoint =
    Point2d Pixels PaneSystem


type alias PaneVector =
    Vector2d Pixels PaneSystem


type LevelZeroPixels
    = LevelZeroPixels


type alias ReductionRateUnit =
    Rate LevelZeroPixels Pixels


type alias ReductionRate =
    Quantity Float ReductionRateUnit
