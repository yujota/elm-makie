module Makie.Internal.Makie exposing
    ( Action(..)
    , Camera(..)
    , CameraAction(..)
    , CameraRecord
    , Contents(..)
    , Event(..)
    , EventMode(..)
    , EventStatus(..)
    , EventStatusRecord
    , Image(..)
    , ImagePoint
    , ImageSystem
    , ImageVector
    , Makie(..)
    , MakieRecord
    , PanePoint
    , PaneSystem
    , PaneVector
    , PointerEvent(..)
    , ReductionRate
    , SingleImageCanvasContentsRecord
    , WheelEvent(..)
    , fromPanePoint
    , fromPaneVector
    , imagePixels
    , imagePoint
    , imageVector
    , inReductionRate
    , initialEventStatus
    , panePoint
    , paneVector
    , reductionRate
    , requestRendering
    , toPanePoint
    , toPaneVector
    )

import Angle exposing (Angle)
import Canvas exposing (Renderable)
import Canvas.Texture exposing (Texture)
import Frame2d exposing (Frame2d)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate(..))
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type Makie
    = Makie MakieRecord


type alias MakieRecord =
    { event : EventStatusRecord
    , paneWidth : Int
    , paneHeight : Int
    , imageWidth : Int
    , imageHeight : Int
    , camera : CameraRecord
    , contents : Contents
    }


type Image
    = Image { src : String, name : String, width : Int, height : Int }


type Contents
    = SingleImageCanvasContents SingleImageCanvasContentsRecord



-- Events


type Event
    = PointerEventVariant PointerEvent
    | WheelEventVariant WheelEvent
    | RefreshPane Posix
    | SingleImageCanvasTextureLoaded (Maybe Texture)


type PointerEvent
    = OnDown Pointer.Event
    | OnMove Pointer.Event
    | OnUp Pointer.Event
    | OnCancel Pointer.Event
    | OnOut Pointer.Event
    | OnLeave Pointer.Event


type WheelEvent
    = OnWheel Wheel.Event


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


initialEventStatus : EventStatusRecord
initialEventStatus =
    { mode = ZeroPointer }



-- Actions


type Action
    = CameraActionVariant CameraAction
    | AnnotationActionVariant AnnotationAction
    | NoAction



-- Camera


type Camera
    = Camera CameraRecord


type alias CameraRecord =
    { imageFrame : ImageFrame
    , reductionRate : ReductionRate
    }


type CameraAction
    = Move PaneVector
    | Zoom PanePoint ReductionRate
    | Resize { width : PanePixels, height : PanePixels }
    | Rotate RotationPoint RotationSpec


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



-- Annotation


type AnnotationAction
    = No



-- View
-- Canvas


type alias SingleImageCanvasContentsRecord =
    { texture : Maybe Texture
    , src : String
    , renderables : List Renderable
    , isRenderingRequested : Bool
    }


requestRendering : Makie -> Makie
requestRendering m =
    m



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


type alias ImageVector =
    Vector2d ImageSystemPixels ImageSystem


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
