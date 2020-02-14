module Makie.Internal.Event exposing (Msg, eventList, manage)

import Angle
import Direction2d exposing (Direction2d)
import Html exposing (Attribute)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Json.Decode as D
import LineSegment2d
import Makie.Internal.Annotation as Annotation
import Makie.Internal.Camera as Camera
import Makie.Internal.Makie as M
import Makie.Types.Geometry as G
import Makie.Types.State as S
import Pixels
import Point2d
import Quantity
import Vector2d


type Msg
    = DesktopMsg DesktopEventType
    | MobileMsg MobileEventType


eventList : (Msg -> msg) -> S.State msg -> List (Attribute msg)
eventList lifter _ =
    -- TODO Mouse and Touch should be replaced with Pointer
    [ onMouseMoveEvent lifter
    , onWheelEvent lifter
    , Mouse.onDown ((\e -> OnMouseDown (e.offsetPos |> (\( a, b ) -> G.cPoint a b))) >> DesktopMsg >> lifter)
    , Mouse.onUp (always OnMouseUp >> DesktopMsg >> lifter)
    , Mouse.onLeave (always OnMouseLeave >> DesktopMsg >> lifter)
    , Touch.onStart (OnTouchStart >> MobileMsg >> lifter)
    , Touch.onMove (OnTouchMove >> MobileMsg >> lifter)
    , Touch.onEnd (OnTouchEnd >> MobileMsg >> lifter)
    , Touch.onCancel (OnTouchCancel >> MobileMsg >> lifter)
    ]


manage : S.Params -> Msg -> S.State msg -> ( S.State msg, List M.Query )
manage params msg state =
    case msg of
        DesktopMsg desktopEvent ->
            manageDesktop params desktopEvent state

        MobileMsg mobileEvent ->
            manageMobile params mobileEvent state



-- Private


manageDesktop : S.Params -> DesktopEventType -> S.State msg -> ( S.State msg, List M.Query )
manageDesktop params desktopEvent state =
    case desktopEvent of
        OnMouseDown p ->
            { state | interaction = state.interaction |> (\r -> { r | isDragging = True }) }
                |> (\s -> ( Annotation.eventStart params p s, [] ))

        OnMouseUp ->
            { state | interaction = state.interaction |> (\r -> { r | isDragging = False }) }
                |> Annotation.eventEnd

        OnMouseLeave ->
            { state | interaction = state.interaction |> (\r -> { r | isDragging = False }) }
                |> (\s -> ( Annotation.eventCancel s, [] ))

        OnMouseMove { movementX, movementY, shiftKey, offsetX, offsetY } ->
            case ( state.interaction.isDragging, shiftKey ) of
                ( True, True ) ->
                    -- MEMO: Camera rotation
                    let
                        center =
                            Point2d.pixels (toFloat state.width / 2) (toFloat state.height / 2)

                        maybeAngleA =
                            Point2d.pixels offsetX offsetY
                                |> Direction2d.from center
                                |> Maybe.map Direction2d.toAngle

                        maybeAngleB =
                            Point2d.pixels (offsetX + movementX) (offsetY + movementY)
                                |> Direction2d.from center
                                |> Maybe.map Direction2d.toAngle
                    in
                    case ( maybeAngleA, maybeAngleB ) of
                        ( Just angleA, Just angleB ) ->
                            { state
                                | cameraActions =
                                    Camera.rotateByCenter state.currentTime
                                        { rotation = Camera.RelativeAngle <| Quantity.minus angleA angleB }
                                        :: state.cameraActions
                            }
                                |> (\s -> ( s, [] ))

                        ( _, _ ) ->
                            ( state, [] )

                ( True, False ) ->
                    case state.mode of
                        S.BrowseMode ->
                            -- MEMO: Camera move
                            { state
                                | cameraActions =
                                    Camera.move state.currentTime
                                        { dx = Pixels.pixels (-1 * movementX), dy = Pixels.pixels (-1 * movementY) }
                                        :: state.cameraActions
                            }
                                |> (\s -> ( s, [] ))

                        S.AnnotationMode _ ->
                            Annotation.eventMove (G.cPoint offsetX offsetY) state
                                |> (\s -> ( s, [] ))

                ( _, _ ) ->
                    ( state, [] )

        OnWheelEvent r ->
            -- TODO: Implement Track Pad mode
            let
                zoomType =
                    if r.deltaY < 0 then
                        Camera.ZoomIn

                    else
                        Camera.ZoomOut
            in
            { state
                | cameraActions =
                    Camera.zoom state.currentTime
                        { x = Pixels.pixels r.offsetX, y = Pixels.pixels r.offsetY, zoomType = zoomType }
                        :: state.cameraActions
            }
                |> (\s -> ( s, [] ))


manageMobile : S.Params -> MobileEventType -> S.State msg -> ( S.State msg, List M.Query )
manageMobile params mobileEvent state =
    let
        toPoint ( a, b ) =
            Point2d.pixels a b

        updateTouch newTouch =
            { state | interaction = state.interaction |> (\r -> { r | touch = newTouch }) }

        updateCameraActions newAction oldState =
            { oldState | cameraActions = newAction :: oldState.cameraActions }

        noQuery s =
            ( s, [] )
    in
    case state.interaction.touch of
        S.NoTouch ->
            case mobileEvent of
                OnTouchStart event ->
                    case event.changedTouches of
                        [ t ] ->
                            let
                                p =
                                    toPoint t.clientPos
                            in
                            S.SingleTouch t.identifier p
                                |> updateTouch
                                |> (\s -> ( Annotation.eventStart params p s, [] ))

                        [ t1, t2 ] ->
                            S.DoubleTouch
                                { idA = t1.identifier
                                , idB = t2.identifier
                                , pointA = t1.clientPos |> toPoint
                                , pointB = t2.clientPos |> toPoint
                                }
                                |> updateTouch
                                |> noQuery

                        _ ->
                            ( state, [] )

                _ ->
                    ( state, [] )

        S.SingleTouch oldId oldPoint ->
            case mobileEvent of
                OnTouchStart event ->
                    case event.changedTouches of
                        [ t ] ->
                            if t.identifier == oldId then
                                let
                                    p =
                                        toPoint t.clientPos
                                in
                                S.SingleTouch t.identifier p
                                    |> updateTouch
                                    |> (\s -> ( Annotation.eventStart params p s, [] ))

                            else
                                S.DoubleTouch
                                    { idA = t.identifier
                                    , idB = oldId
                                    , pointA = t.clientPos |> toPoint
                                    , pointB = oldPoint
                                    }
                                    |> updateTouch
                                    |> noQuery

                        [ t1, t2 ] ->
                            S.DoubleTouch
                                { idA = t1.identifier
                                , idB = t2.identifier
                                , pointA = t1.clientPos |> toPoint
                                , pointB = t2.clientPos |> toPoint
                                }
                                |> updateTouch
                                |> noQuery

                        _ ->
                            ( state, [] )

                OnTouchMove event ->
                    case event.changedTouches of
                        [ t ] ->
                            let
                                newPoint =
                                    t.clientPos |> toPoint

                                -- MEMO: タッチの動きの差分とカメラの向きが逆になる.
                                diff =
                                    Vector2d.from newPoint oldPoint

                                action =
                                    Camera.move state.currentTime
                                        { dx = Vector2d.xComponent diff, dy = Vector2d.yComponent diff }
                            in
                            if t.identifier == oldId then
                                case state.mode of
                                    S.BrowseMode ->
                                        -- MEMO: Camera move
                                        S.SingleTouch t.identifier (toPoint t.clientPos)
                                            |> updateTouch
                                            |> updateCameraActions action
                                            |> (\s -> ( s, [] ))

                                    S.AnnotationMode _ ->
                                        S.SingleTouch t.identifier (toPoint t.clientPos)
                                            |> updateTouch
                                            |> Annotation.eventMove newPoint
                                            |> (\s -> ( s, [] ))

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchEnd _ ->
                    updateTouch S.NoTouch
                        |> Annotation.eventEnd

                OnTouchCancel _ ->
                    updateTouch S.NoTouch
                        |> (\s -> ( Annotation.eventCancel s, [] ))

        S.DoubleTouch old ->
            case mobileEvent of
                OnTouchStart _ ->
                    ( state, [] )

                OnTouchMove event ->
                    let
                        baseLength =
                            Vector2d.from old.pointA old.pointB
                                |> Vector2d.length
                                |> Quantity.abs

                        -- MEMO: DoubleTouchを意味のあるZoomTouchかRotateAndZoomTouchに変更して, 再帰的に関数を呼ぶ
                        upgradeTouch maybeTargetDirection1 maybeTargetDirection2 =
                            case
                                toBeRotate
                                    (Direction2d.from old.pointA old.pointB)
                                    maybeTargetDirection1
                                    maybeTargetDirection2
                            of
                                True ->
                                    S.RotateAndZoomTouch
                                        { idA = old.idA
                                        , idB = old.idB
                                        , pointA = old.pointA
                                        , pointB = old.pointB
                                        , initialLength = baseLength
                                        , baseReductionRate = Camera.getReductionRate state.camera
                                        , baseAngle = Camera.getAngle state.camera |> Quantity.negate
                                        , initialAngle =
                                            Direction2d.from old.pointA old.pointB
                                                |> Maybe.map Direction2d.toAngle
                                                |> Maybe.withDefault (Angle.degrees 0)
                                        , lastAngle =
                                            Direction2d.from old.pointA old.pointB
                                                |> Maybe.map Direction2d.toAngle
                                                |> Maybe.withDefault (Angle.degrees 0)
                                        }
                                        |> updateTouch
                                        |> manageMobile params (OnTouchMove event)

                                False ->
                                    S.ZoomTouch
                                        { idA = old.idA
                                        , idB = old.idB
                                        , pointA = old.pointA
                                        , pointB = old.pointB
                                        , initialLength = baseLength
                                        , baseReductionRate = Camera.getReductionRate state.camera
                                        }
                                        |> updateTouch
                                        |> manageMobile params (OnTouchMove event)
                    in
                    case event.changedTouches of
                        [ t ] ->
                            let
                                p =
                                    t.clientPos |> toPoint
                            in
                            if t.identifier == old.idA then
                                upgradeTouch (Direction2d.from old.pointA p) Nothing

                            else if t.identifier == old.idB then
                                upgradeTouch (Direction2d.from old.pointB p) Nothing

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            let
                                ( p1, p2 ) =
                                    ( t1.clientPos |> toPoint, t2.clientPos |> toPoint )
                            in
                            if t1.identifier == old.idA && t2.identifier == old.idB then
                                upgradeTouch (Direction2d.from old.pointA p1) (Direction2d.from old.pointB p2)

                            else if t2.identifier == old.idA && t1.identifier == old.idB then
                                upgradeTouch (Direction2d.from old.pointA p2) (Direction2d.from old.pointB p1)

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchEnd event ->
                    case event.changedTouches of
                        [ t ] ->
                            if t.identifier == old.idA then
                                S.SingleTouch old.idB old.pointB |> updateTouch |> noQuery

                            else if t.identifier == old.idB then
                                S.SingleTouch old.idA old.pointA |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            if t1.identifier == old.idA && t2.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else if t2.identifier == old.idA && t1.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchCancel e ->
                    manageMobile params (OnTouchEnd e) state

        S.ZoomTouch old ->
            case mobileEvent of
                OnTouchStart _ ->
                    ( state, [] )

                OnTouchMove event ->
                    let
                        -- MEMO: DoubleTouchを意味のあるZoomTouchかRotateAndZoomTouchに変更して, 再帰的に関数を呼ぶ
                        update newIdA newIdB newPointA newPointB =
                            let
                                newTouch =
                                    S.ZoomTouch
                                        { idA = newIdA
                                        , idB = newIdB
                                        , pointA = newPointA
                                        , pointB = newPointB
                                        , initialLength = old.initialLength
                                        , baseReductionRate = old.baseReductionRate
                                        }

                                newLine =
                                    LineSegment2d.from newPointA newPointB

                                newReductionRate =
                                    old.baseReductionRate
                                        |> Quantity.multiplyBy
                                            (Quantity.ratio
                                                old.initialLength
                                                (LineSegment2d.length newLine)
                                            )

                                zoomPoint =
                                    LineSegment2d.midpoint newLine

                                newAction =
                                    Camera.zoom state.currentTime
                                        { x = Point2d.xCoordinate zoomPoint
                                        , y = Point2d.yCoordinate zoomPoint
                                        , zoomType = Camera.ZoomWithReductionRate newReductionRate
                                        }
                            in
                            updateTouch newTouch |> updateCameraActions newAction |> noQuery
                    in
                    case event.changedTouches of
                        [ t ] ->
                            let
                                p =
                                    t.clientPos |> toPoint
                            in
                            if t.identifier == old.idA then
                                update old.idA old.idB p old.pointB

                            else if t.identifier == old.idB then
                                update old.idA old.idB old.pointA p

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            let
                                ( p1, p2 ) =
                                    ( t1.clientPos |> toPoint, t2.clientPos |> toPoint )
                            in
                            update t1.identifier t2.identifier p1 p2

                        _ ->
                            ( state, [] )

                OnTouchEnd event ->
                    case event.changedTouches of
                        [ t ] ->
                            if t.identifier == old.idA then
                                S.SingleTouch old.idB old.pointB |> updateTouch |> noQuery

                            else if t.identifier == old.idB then
                                S.SingleTouch old.idA old.pointA |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            if t1.identifier == old.idA && t2.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else if t2.identifier == old.idA && t1.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchCancel e ->
                    manageMobile params (OnTouchEnd e) state

        S.RotateAndZoomTouch old ->
            case mobileEvent of
                OnTouchStart _ ->
                    ( state, [] )

                OnTouchMove event ->
                    let
                        -- MEMO: DoubleTouchを意味のあるZoomTouchかRotateAndZoomTouchに変更して, 再帰的に関数を呼ぶ
                        update newIdA newIdB newPointA newPointB =
                            let
                                newTouch =
                                    case maybeNewAngle of
                                        Just a ->
                                            S.RotateAndZoomTouch
                                                { old
                                                    | idA = newIdA
                                                    , idB = newIdB
                                                    , pointA = newPointA
                                                    , pointB = newPointB
                                                    , lastAngle = a
                                                }

                                        Nothing ->
                                            S.RotateAndZoomTouch
                                                { old
                                                    | idA = newIdA
                                                    , idB = newIdB
                                                    , pointA = newPointA
                                                    , pointB = newPointB
                                                }

                                newLine =
                                    LineSegment2d.from newPointA newPointB

                                maybeNewAngle =
                                    Direction2d.from newPointA newPointB
                                        |> Maybe.map Direction2d.toAngle

                                newReductionRate =
                                    old.baseReductionRate
                                        |> Quantity.multiplyBy
                                            (Quantity.ratio
                                                old.initialLength
                                                (LineSegment2d.length newLine)
                                            )

                                zoomPoint =
                                    LineSegment2d.midpoint newLine

                                zoomAction =
                                    Camera.zoom state.currentTime
                                        { x = Point2d.xCoordinate zoomPoint
                                        , y = Point2d.yCoordinate zoomPoint
                                        , zoomType = Camera.ZoomWithReductionRate newReductionRate
                                        }
                            in
                            case maybeNewAngle of
                                Just a ->
                                    let
                                        rotateAction =
                                            Camera.rotate state.currentTime
                                                { x = Point2d.xCoordinate zoomPoint
                                                , y = Point2d.yCoordinate zoomPoint
                                                , rotation = Camera.RelativeAngle (Quantity.minus old.lastAngle a)
                                                }
                                    in
                                    updateTouch newTouch
                                        |> updateCameraActions zoomAction
                                        |> updateCameraActions rotateAction
                                        |> noQuery

                                Nothing ->
                                    updateTouch newTouch |> updateCameraActions zoomAction |> noQuery
                    in
                    case event.changedTouches of
                        [ t ] ->
                            let
                                p =
                                    t.clientPos |> toPoint
                            in
                            if t.identifier == old.idA then
                                update old.idA old.idB p old.pointB

                            else if t.identifier == old.idB then
                                update old.idA old.idB old.pointA p

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            let
                                ( p1, p2 ) =
                                    ( t1.clientPos |> toPoint, t2.clientPos |> toPoint )
                            in
                            if t1.identifier == old.idA && t2.identifier == old.idB then
                                update t1.identifier t2.identifier p1 p2

                            else if t2.identifier == old.idA && t1.identifier == old.idB then
                                -- MEMO : 角度が反転してしまうため, この条件分けが必要
                                update t2.identifier t1.identifier p2 p1

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchEnd event ->
                    case event.changedTouches of
                        [ t ] ->
                            if t.identifier == old.idA then
                                S.SingleTouch old.idB old.pointB |> updateTouch |> noQuery

                            else if t.identifier == old.idB then
                                S.SingleTouch old.idA old.pointA |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        [ t1, t2 ] ->
                            if t1.identifier == old.idA && t2.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else if t2.identifier == old.idA && t1.identifier == old.idB then
                                S.NoTouch |> updateTouch |> noQuery

                            else
                                ( state, [] )

                        _ ->
                            ( state, [] )

                OnTouchCancel e ->
                    manageMobile params (OnTouchEnd e) state


toBeRotate :
    Maybe (Direction2d G.CanvasSystem)
    -> Maybe (Direction2d G.CanvasSystem)
    -> Maybe (Direction2d G.CanvasSystem)
    -> Bool
toBeRotate maybeBase maybeDirA maybeDirB =
    let
        tolerance =
            60

        inRangeA a =
            a |> Angle.inDegrees |> floor |> modBy 360 |> (\i -> 90 - tolerance <= i && i <= 90 + tolerance)

        inRangeB a =
            a |> Angle.inDegrees |> floor |> modBy 360 |> (\i -> 270 - tolerance <= i && i <= 270 + tolerance)
    in
    case ( maybeBase, maybeDirA ) of
        ( Just base, Just dirA ) ->
            let
                diffA =
                    Quantity.minus (Direction2d.toAngle base) (Direction2d.toAngle dirA)
            in
            case maybeDirB of
                Just dirB ->
                    let
                        diffB =
                            Quantity.minus (Direction2d.toAngle base) (Direction2d.toAngle dirB)
                    in
                    (inRangeA diffA && inRangeB diffB) || (inRangeB diffA && inRangeA diffB)

                Nothing ->
                    inRangeA diffA || inRangeB diffA

        ( _, _ ) ->
            False


type DesktopEventType
    = OnMouseDown G.CPoint
    | OnMouseUp
    | OnMouseLeave
    | OnMouseMove MouseMoveEvent
    | OnWheelEvent MouseWheelEvent


type alias MouseMoveEvent =
    { movementX : Float
    , movementY : Float
    , shiftKey : Bool
    , offsetX : Float
    , offsetY : Float
    }


type alias MouseWheelEvent =
    { offsetX : Float
    , offsetY : Float
    , deltaY : Float
    , deltaX : Float
    , ctrlKey : Bool
    }


type MobileEventType
    = OnTouchStart Touch.Event
    | OnTouchMove Touch.Event
    | OnTouchEnd Touch.Event
    | OnTouchCancel Touch.Event


onMouseMoveEvent : (Msg -> msg) -> Attribute msg
onMouseMoveEvent lifter =
    let
        mouseMoveDecoder : D.Decoder MouseMoveEvent
        mouseMoveDecoder =
            D.map5 (\a b c d e -> { movementX = a, movementY = b, shiftKey = c, offsetX = d, offsetY = e })
                (D.at [ "movementX" ] D.float)
                (D.at [ "movementY" ] D.float)
                (D.at [ "shiftKey" ] D.bool)
                (D.at [ "offsetX" ] D.float)
                (D.at [ "offsetY" ] D.float)
    in
    Html.Events.on "mousemove" (D.map (OnMouseMove >> DesktopMsg >> lifter) mouseMoveDecoder)


onWheelEvent : (Msg -> msg) -> Attribute msg
onWheelEvent lifter =
    -- TODO: Propagation をOffにしないと, ページもスクロールされる.
    let
        wheelEventDecoder : D.Decoder MouseWheelEvent
        wheelEventDecoder =
            D.map5 (\a b c d e -> { offsetX = a, offsetY = b, deltaY = c, deltaX = d, ctrlKey = e })
                (D.at [ "offsetX" ] D.float)
                (D.at [ "offsetY" ] D.float)
                (D.at [ "deltaY" ] D.float)
                (D.at [ "deltaX" ] D.float)
                (D.at [ "ctrlKey" ] D.bool)

        customEventDecoder : D.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
        customEventDecoder =
            D.map
                (\m ->
                    { message = m
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                (D.map (OnWheelEvent >> DesktopMsg >> lifter)
                    wheelEventDecoder
                )
    in
    Html.Events.custom "wheel" customEventDecoder
