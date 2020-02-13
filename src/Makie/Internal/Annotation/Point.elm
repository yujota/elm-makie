module Makie.Internal.Annotation.Point exposing (..)

import BoundingBox2d
import Canvas as C
import Canvas.Settings as CSettings
import Color exposing (Color)
import Makie.Types.Annotation as A
import Makie.Types.Geometry as G
import Point2d
import Quantity


start : G.CPoint -> A.PointHandleType
start p =
    A.PointHandleMove p


getHandle :
    { toCanvasPoint : G.PPoint -> G.CPoint
    , fromCanvasPoint : G.CPoint -> G.PPoint
    , currentPoint : G.CPoint
    , tolerance : G.CanvasPixels
    , reductionRate : G.ReductionRate
    }
    -> A.PointProperty
    -> Maybe A.PointHandleType
getHandle { fromCanvasPoint, currentPoint, tolerance, reductionRate } ant =
    let
        pPoint =
            fromCanvasPoint currentPoint
    in
    if isTouched { tolerance = tolerance, reductionRate = reductionRate } pPoint ant then
        currentPoint |> A.PointHandleMove |> Just

    else
        Nothing


getBoundingBox : A.PointProperty -> G.PBoundingBox
getBoundingBox ant =
    BoundingBox2d.singleton ant.point


updateHandle : G.CPoint -> A.PointHandleType -> A.PointHandleType
updateHandle p _ =
    A.PointHandleMove p


finish :
    (G.CPoint -> G.PPoint)
    -> A.PointHandleType
    -> Result String A.PointProperty
finish fromCanvasPoint hdl =
    case hdl of
        A.PointHandleMove p ->
            { point = fromCanvasPoint p } |> Ok


isTouched : { tolerance : G.CanvasPixels, reductionRate : G.ReductionRate } -> G.PPoint -> A.PointProperty -> Bool
isTouched { tolerance, reductionRate } p ant =
    let
        touchRadius : G.PathologyPixels
        touchRadius =
            tolerance
                |> Quantity.at reductionRate
    in
    Point2d.equalWithin touchRadius ant.point p


render :
    { toCanvasPoint : G.PPoint -> G.CPoint, color : Color, radius : Float }
    -> A.PointProperty
    -> List C.Renderable
render { toCanvasPoint, color, radius } annotation =
    let
        { x, y } =
            annotation.point |> toCanvasPoint |> Point2d.toPixels
    in
    [ C.shapes [ CSettings.fill color ] [ C.circle ( x, y ) radius ] ]


renderHandle :
    { color : Color, radius : Float }
    -> A.PointHandleType
    -> List C.Renderable
renderHandle { color, radius } handle =
    case handle of
        A.PointHandleMove p ->
            let
                { x, y } =
                    Point2d.toPixels p
            in
            [ C.shapes [ CSettings.fill color ] [ C.circle ( x, y ) radius ] ]
