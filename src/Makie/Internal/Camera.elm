module Makie.Internal.Camera exposing
    ( Action
    , Camera
    , RotationType(..)
    , ZoomType(..)
    , canvasFrame
    , canvasSize
    , create
    , fitToScreen
    , fromCanvasPoint
    , getAngle
    , getReductionRate
    , isVisible
    , move
    , origin
    , resize
    , rotate
    , rotateByCenter
    , toCanvasPoint
    , update
    , zoom
    , zoomByCenter
    , zooming
    )

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Makie.Types.Camera
import Makie.Types.Geometry as G
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type Camera
    = Camera Makie.Types.Camera.Camera


type Action
    = Action { timeStamp : Posix, action : ActionType }


type ActionType
    = Move { dx : G.CanvasPixels, dy : G.CanvasPixels }
    | Resize { width : G.CanvasPixels, height : G.CanvasPixels }
    | ZoomByCenter { zoomType : ZoomType }
    | Zoom { zoomType : ZoomType, x : G.CanvasPixels, y : G.CanvasPixels }
    | Rotate { rotation : RotationType, x : G.CanvasPixels, y : G.CanvasPixels }
    | RotateByCenter { rotation : RotationType }
    | FitToScreen


type ZoomType
    = ZoomIn
    | ZoomOut
    | ZoomWithReductionRate G.ReductionRate


type RotationType
    = RelativeAngle Angle
    | AbsoluteAngle Angle



-- Public functions for App.Main


{-| Origin is center Point
-}
origin : Makie.Types.Camera.Constraint -> Camera
origin constraint =
    let
        halfWidth =
            constraint.lv0Width |> Quantity.half

        halfHeight =
            constraint.lv0Height |> Quantity.half

        center =
            Point2d.xy halfWidth halfHeight
    in
    Camera
        { center = center -- will be deprecated
        , pHalfWidth = G.pathologyPixels 0
        , pHalfHeight = G.pathologyPixels 0
        , width = Pixels.pixels 0
        , height = Pixels.pixels 0
        , frame = Frame2d.atOrigin
        , reductionRate = G.reductionRate 1
        , lastUpdated = Time.millisToPosix 0
        , constraint = constraint
        , angle = Angle.degrees 0
        }


{-| Origin is center Point
-}
create :
    Makie.Types.Camera.Constraint
    -> { width : G.CanvasPixels, height : G.CanvasPixels }
    -> Camera
create constraint { width, height } =
    let
        halfWidth =
            constraint.lv0Width |> Quantity.half

        halfHeight =
            constraint.lv0Height |> Quantity.half

        center =
            Point2d.xy halfWidth halfHeight
    in
    Camera
        { center = center -- will be deprecated
        , pHalfWidth = G.pathologyPixels 0
        , pHalfHeight = G.pathologyPixels 0
        , width = width
        , height = height
        , frame = Frame2d.atOrigin
        , reductionRate = G.reductionRate 1
        , lastUpdated = Time.millisToPosix 0
        , constraint = constraint
        , angle = Angle.degrees 0
        }
        |> applyFitToScreen


{-| Update camera by given operations
(the first of list is the newest operation and the last is the oldest)
-}
update : List Action -> Camera -> Camera
update actions camera =
    List.foldr applyAction camera actions


move : Posix -> { dx : G.CanvasPixels, dy : G.CanvasPixels } -> Action
move timeStamp r =
    Action { action = Move r, timeStamp = timeStamp }


{-| ここで設定されたreductionRateに変更される
-}
zoom : Posix -> { x : G.CanvasPixels, y : G.CanvasPixels, zoomType : ZoomType } -> Action
zoom timeStamp r =
    Action { action = Zoom r, timeStamp = timeStamp }


zoomByCenter : Posix -> { zoomType : ZoomType } -> Action
zoomByCenter timeStamp r =
    Action { action = ZoomByCenter r, timeStamp = timeStamp }


rotate : Posix -> { x : G.CanvasPixels, y : G.CanvasPixels, rotation : RotationType } -> Action
rotate timeStamp r =
    Action { action = Rotate r, timeStamp = timeStamp }


rotateByCenter : Posix -> { rotation : RotationType } -> Action
rotateByCenter timeStamp r =
    Action { action = RotateByCenter r, timeStamp = timeStamp }


resize : Posix -> { width : G.CanvasPixels, height : G.CanvasPixels } -> Action
resize timeStamp args =
    Action { action = Resize args, timeStamp = timeStamp }


fitToScreen : Posix -> Action
fitToScreen timeStamp =
    Action { action = FitToScreen, timeStamp = timeStamp }



-- Accessors


getReductionRate : Camera -> G.ReductionRate
getReductionRate (Camera data) =
    data.reductionRate


canvasFrame : Camera -> G.CFrame
canvasFrame (Camera data) =
    data.frame


canvasSize : Camera -> { width : G.CanvasPixels, height : G.CanvasPixels }
canvasSize (Camera data) =
    { width = data.width, height = data.height }


isVisible : Camera -> G.PPoint -> Bool
isVisible (Camera data) p =
    let
        c =
            toCanvasPoint (Camera data) p

        topLeft =
            G.cPoint 0 0

        bottomRight =
            Point2d.xy data.width data.height
    in
    BoundingBox2d.from topLeft bottomRight
        |> BoundingBox2d.contains c



-- functions for Pathology modules


zooming : Camera -> Bool
zooming _ =
    -- TODO
    False


toCanvasPoint : Camera -> G.PPoint -> G.CPoint
toCanvasPoint (Camera data) pPoint =
    let
        centerToPointInP =
            Vector2d.from data.center pPoint

        centerToPointInC =
            centerToPointInP
                |> Vector2d.at_ data.reductionRate
                |> Vector2d.relativeTo data.frame

        centerInC =
            Point2d.xy (Quantity.half data.width) (Quantity.half data.height)
    in
    centerInC |> Point2d.translateBy centerToPointInC


fromCanvasPoint : Camera -> G.CPoint -> G.PPoint
fromCanvasPoint (Camera data) cPoint =
    -- TODO: Need to be tested
    cPoint |> Point2d.placeIn data.frame |> Point2d.at data.reductionRate


getAngle : Camera -> Angle
getAngle (Camera data) =
    data.angle



-- Private functions


applyAction : Action -> Camera -> Camera
applyAction (Action { timeStamp, action }) camera =
    (\(Camera d) -> Camera { d | lastUpdated = timeStamp }) <|
        case action of
            Move r ->
                applyMove r camera

            Resize r ->
                applyResize r camera

            ZoomByCenter r ->
                applyZoomByCenter r camera

            Zoom r ->
                applyZoom r camera

            Rotate r ->
                applyRotate r camera

            RotateByCenter r ->
                applyRotateByCenter r camera

            FitToScreen ->
                applyFitToScreen camera


applyMove : { dx : G.CanvasPixels, dy : G.CanvasPixels } -> Camera -> Camera
applyMove { dx, dy } (Camera data) =
    let
        diffInC =
            { xVec = Vector2d.rotateBy data.angle (Vector2d.xy dx Quantity.zero)
            , yVec = Vector2d.rotateBy data.angle (Vector2d.xy Quantity.zero dy)
            }
                |> (\r -> Vector2d.plus r.xVec r.yVec)

        newFrame =
            Frame2d.translateBy diffInC data.frame

        newCenterInP =
            Point2d.xy (Quantity.half data.width) (Quantity.half data.height)
                |> Point2d.placeIn newFrame
                |> Point2d.at data.reductionRate
    in
    Camera { data | center = newCenterInP, frame = newFrame }


applyZoom :
    { x : G.CanvasPixels, y : G.CanvasPixels, zoomType : ZoomType }
    -> Camera
    -> Camera
applyZoom { x, y, zoomType } (Camera data) =
    let
        newReductionRate =
            case zoomType of
                ZoomIn ->
                    Quantity.multiplyBy 0.666 data.reductionRate

                ZoomOut ->
                    Quantity.multiplyBy 1.5 data.reductionRate

                ZoomWithReductionRate rRate ->
                    rRate

        newPathologyOriginToZoomPoint : Vector2d Pixels G.PathologySystem
        newPathologyOriginToZoomPoint =
            Vector2d.from Point2d.origin (Point2d.xy x y |> Point2d.placeIn data.frame)
                |> Vector2d.scaleBy (Quantity.ratio data.reductionRate newReductionRate)

        zoomPointToCanvasOrigin : Vector2d Pixels G.PathologySystem
        zoomPointToCanvasOrigin =
            Vector2d.from (Point2d.xy x y |> Point2d.placeIn data.frame) (Point2d.origin |> Point2d.placeIn data.frame)

        newCanvasOrigin =
            Point2d.translateBy (Vector2d.plus newPathologyOriginToZoomPoint zoomPointToCanvasOrigin) Point2d.origin

        newFrame =
            Frame2d.moveTo newCanvasOrigin data.frame

        newCenter =
            Point2d.xy (Quantity.half data.width) (Quantity.half data.height)
                |> Point2d.placeIn newFrame
                |> Point2d.at newReductionRate
    in
    Camera
        { data
            | frame = newFrame
            , center = newCenter
            , reductionRate = newReductionRate
        }


applyZoomByCenter :
    { zoomType : ZoomType }
    -> Camera
    -> Camera
applyZoomByCenter { zoomType } (Camera data) =
    applyZoom { zoomType = zoomType, x = Quantity.half data.width, y = Quantity.half data.height } (Camera data)


applyRotate :
    { rotation : RotationType, x : G.CanvasPixels, y : G.CanvasPixels }
    -> Camera
    -> Camera
applyRotate { rotation, x, y } (Camera data) =
    let
        angle =
            case rotation of
                RelativeAngle a ->
                    a

                AbsoluteAngle a ->
                    -- TODO: バグあり, 使うな!
                    Quantity.minus data.angle a

        canvasOrigin : Point2d Pixels G.PathologySystem
        canvasOrigin =
            Point2d.origin |> Point2d.placeIn data.frame

        rotationPoint : Point2d Pixels G.PathologySystem
        rotationPoint =
            Point2d.xy x y
                |> Point2d.placeIn data.frame

        rotPointToOrigin : Vector2d Pixels G.PathologySystem
        rotPointToOrigin =
            Vector2d.from rotationPoint canvasOrigin
                |> (\v ->
                        { xVec = Vector2d.xy (Vector2d.xComponent v) Quantity.zero
                        , yVec = Vector2d.xy Quantity.zero (Vector2d.yComponent v)
                        }
                   )
                |> (\r ->
                        { xVec = Vector2d.rotateBy (Quantity.negate angle) r.xVec
                        , yVec = Vector2d.rotateBy (Quantity.negate angle) r.yVec
                        }
                   )
                |> (\r -> Vector2d.plus r.xVec r.yVec)

        newFrame : G.CFrame
        newFrame =
            Point2d.translateBy rotPointToOrigin rotationPoint
                |> Frame2d.atPoint
                |> Frame2d.rotateBy (Quantity.minus angle data.angle)

        newCenter =
            Point2d.xy (Quantity.half data.width) (Quantity.half data.height)
                |> Point2d.placeIn newFrame
                |> Point2d.at data.reductionRate
                |> identity
    in
    Camera
        { data
            | frame = newFrame
            , center = newCenter
            , angle = Quantity.minus angle data.angle
        }


applyRotateByCenter :
    { rotation : RotationType }
    -> Camera
    -> Camera
applyRotateByCenter { rotation } (Camera data) =
    applyRotate { rotation = rotation, x = Quantity.half data.width, y = Quantity.half data.height } (Camera data)


applyFitToScreen : Camera -> Camera
applyFitToScreen (Camera data) =
    let
        ( widthRate, heightRate ) =
            ( G.inPathologyPixels data.constraint.lv0Width / Pixels.inPixels data.width
            , G.inPathologyPixels data.constraint.lv0Height / Pixels.inPixels data.height
            )

        centerPoint =
            Point2d.xy
                (Quantity.half data.constraint.lv0Width)
                (Quantity.half data.constraint.lv0Height)
    in
    case widthRate > heightRate of
        True ->
            -- TV画面で洋画を見るときのように画面上下が映らないような場合
            let
                pathologyTopToCanvasTop =
                    (G.inPathologyPixels data.constraint.lv0Height
                        / widthRate
                        - Pixels.inPixels data.height
                    )
                        / 2

                frame =
                    Point2d.pixels 0 pathologyTopToCanvasTop
                        |> Frame2d.atPoint
            in
            Camera { data | center = centerPoint, reductionRate = G.reductionRate widthRate, frame = frame }

        False ->
            let
                pathologyLeftToCanvasLeft =
                    (G.inPathologyPixels data.constraint.lv0Width
                        / heightRate
                        - Pixels.inPixels data.width
                    )
                        / 2

                frame =
                    Point2d.pixels pathologyLeftToCanvasLeft 0
                        |> Frame2d.atPoint
            in
            Camera { data | center = centerPoint, reductionRate = G.reductionRate heightRate, frame = frame }


applyResize : { width : G.CanvasPixels, height : G.CanvasPixels } -> Camera -> Camera
applyResize { width, height } (Camera data) =
    let
        ( pHalfWidth, pHalfHeight ) =
            ( Quantity.at data.reductionRate width |> Quantity.half
            , Quantity.at data.reductionRate height |> Quantity.half
            )

        -- Canvas左上頂点の座標系の原点が, (新しいwidth - 古いwidth) / 2 だけ中心から左上に移動する.
        cDiff =
            Vector2d.xy (Quantity.minus width data.width |> Quantity.half)
                (Quantity.minus height data.height |> Quantity.half)

        frame =
            Frame2d.translateBy cDiff data.frame
    in
    Camera
        { data
            | pHalfWidth = pHalfWidth
            , pHalfHeight = pHalfHeight
            , width = width
            , height = height
            , frame = frame
        }
