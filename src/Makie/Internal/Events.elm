module Makie.Internal.Events exposing (interpret)

import Angle exposing (Angle)
import Direction2d
import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Annotations as Annotations
import Makie.Internal.Canvas
import Makie.Internal.Gestures as Gestures
import Makie.Internal.Makie as M
import Pixels
import Quantity
import Vector2d


interpret : M.Event -> M.MakieRecord -> ( M.MakieRecord, M.Action )
interpret event ({ camera } as m) =
    case event of
        M.PointerEventVariant pointerEvent ->
            interpretPointerEvents pointerEvent m

        M.WheelEventVariant (M.OnWheel ev) ->
            let
                zoomRatio =
                    if ev.deltaY < 0 then
                        2

                    else
                        0.5

                newReductionRate =
                    Quantity.multiplyBy zoomRatio camera.reductionRate

                zoomPoint =
                    ev.mouseEvent |> .offsetPos |> (\( x, y ) -> M.panePoint { x = x, y = y })
            in
            ( m, M.Zoom zoomPoint newReductionRate )

        M.RefreshPane posix ->
            case m.contents of
                M.SingleImageCanvasContents c ->
                    Makie.Internal.Canvas.renderSingleImageCanvas
                        { paneWidth = m.paneWidth
                        , paneHeight = m.paneHeight
                        , imageWidth = m.imageWidth
                        , imageHeight = m.imageHeight
                        , camera = m.camera
                        }
                        c
                        |> (\cnt ->
                                ( { m
                                    | contents = M.SingleImageCanvasContents cnt
                                    , renderedTime = posix
                                  }
                                , M.NoAction
                                )
                           )

        M.SingleImageCanvasTextureLoaded maybeTexture ->
            case m.contents of
                M.SingleImageCanvasContents c ->
                    Makie.Internal.Canvas.handleSingleImageCanvasTextureLoaded maybeTexture c
                        |> (\cnt ->
                                ( { m | contents = M.SingleImageCanvasContents cnt } |> M.requestRendering
                                , M.NoAction
                                )
                           )

        M.OpenLabelEdit uuid ->
            ( m, M.NoAction )

        M.SetMode mode ->
            ( { m | mode = mode }, M.NoAction )



-- Helper functions


interpretPointerEvents : M.PointerEvent -> M.MakieRecord -> ( M.MakieRecord, M.Action )
interpretPointerEvents event ({ gestureModel, paneWidth, paneHeight } as mRecord) =
    let
        ( newGestureModel, gesture ) =
            Gestures.update event gestureModel

        _ =
            Debug.log "gesture & status" ( gesture, newGestureModel )

        m =
            { mRecord | gestureModel = newGestureModel }
    in
    case gesture of
        M.NoGesture ->
            ( m, M.NoAction )

        M.MouseStart r panePoint ->
            mouseStart r panePoint m

        M.MouseOnGoing r lastPoint panePoint ->
            mouseOnGoing r lastPoint panePoint m

        M.MouseEnd r lastPoint panePoint ->
            mouseEnd r lastPoint panePoint m

        M.PenStart panePoint ->
            ( m, M.NoAction )

        M.PenOnGoing lastPoint panePoint ->
            ( m, M.NoAction )

        M.PenEnd lastPoint panePoint ->
            ( m, M.NoAction )

        M.SingleTouchStart record panePoint ->
            ( m, M.NoAction )

        M.SingleTouchOnGoing record lastPoint panePoint ->
            ( m, M.NoAction )

        M.SingleTouchEnd record lastPoint panePoint ->
            ( m, M.NoAction )


mouseStart : { shiftKey : Bool, spaceKey : Bool } -> M.PanePoint -> M.MakieRecord -> ( M.MakieRecord, M.Action )
mouseStart r panePoint ({ camera } as m) =
    let
        startAnnotation f =
            ( { m
                | target =
                    M.TargetCreating
                        { label = m.defaultLabel
                        , notices = M.defaultNotices
                        , handle =
                            M.fromPanePoint camera.reductionRate camera.imageFrame panePoint |> f
                        }
              }
            , M.NoAction
            )
    in
    case m.target of
        M.NoTarget ->
            case getTouchedAnnotation panePoint m of
                Just ( key, ant ) ->
                    -- In case, selecting an annotation
                    ( { m | target = M.TargetSelected key ant, gestureModel = Gestures.clear m.gestureModel }
                    , M.NoAction
                    )

                Nothing ->
                    case ( m.mode, r.spaceKey ) of
                        ( _, True ) ->
                            -- When space key is pressed, app behave like browse mode.
                            ( m, M.NoAction )

                        ( M.BrowseMode, False ) ->
                            ( m, M.NoAction )

                        ( M.PointMode, False ) ->
                            startAnnotation Annotations.startPoint

                        ( M.RectangleMode, False ) ->
                            startAnnotation Annotations.startRectangle

                        ( M.PolygonMode, False ) ->
                            -- TODO
                            ( m, M.NoAction )

                        ( M.SelectionRectangleMode, False ) ->
                            -- TODO
                            ( m, M.NoAction )

        M.TargetCreating _ ->
            -- This case should not be happened.
            ( m, M.NoAction )

        M.TargetSelected targetId targetAnnotation ->
            case getAnnotationHandle panePoint m targetAnnotation of
                Just hdl ->
                    ( { m
                        | target =
                            M.TargetEditing targetId
                                { label = targetAnnotation.label, notices = targetAnnotation.notices, handle = hdl }
                      }
                    , M.NoAction
                    )

                Nothing ->
                    case getTouchedAnnotation panePoint m of
                        Just ( key, ant ) ->
                            -- Change selecting target
                            ( { m | target = M.TargetSelected key ant, gestureModel = Gestures.clear m.gestureModel }
                            , M.NoAction
                            )

                        Nothing ->
                            -- Unselect target
                            ( { m | target = M.NoTarget, gestureModel = Gestures.clear m.gestureModel }
                            , M.NoAction
                            )

        M.TargetEditing targetId targetAnnotation ->
            -- This case should not be happened.
            ( m, M.NoAction )


mouseOnGoing :
    { shiftKey : Bool, spaceKey : Bool }
    -> M.PanePoint
    -> M.PanePoint
    -> M.MakieRecord
    -> ( M.MakieRecord, M.Action )
mouseOnGoing r lastPoint panePoint ({ camera } as m) =
    let
        cameraMove =
            Vector2d.from lastPoint panePoint |> M.Move

        updateHandle hdl =
            Annotations.updateHandle (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint) hdl
    in
    case m.target of
        M.NoTarget ->
            case ( m.mode, r.spaceKey ) of
                ( _, True ) ->
                    -- When space key is pressed, app behave like browse mode.
                    ( m, cameraMove )

                ( M.BrowseMode, False ) ->
                    if r.shiftKey then
                        let
                            centerPoint =
                                M.panePoint { x = toFloat m.paneWidth / 2, y = toFloat m.paneHeight / 2 }
                        in
                        case calcAngle centerPoint lastPoint panePoint of
                            Just a ->
                                ( m, M.Rotate centerPoint a )

                            Nothing ->
                                ( m, M.NoAction )

                    else
                        ( m, cameraMove )

                _ ->
                    ( m, M.NoAction )

        M.TargetCreating handleRecord ->
            ( { m | target = M.TargetCreating { handleRecord | handle = updateHandle handleRecord.handle } }
            , M.NoAction
            )

        M.TargetEditing targetId handleRecord ->
            ( { m | target = M.TargetEditing targetId { handleRecord | handle = updateHandle handleRecord.handle } }
            , M.NoAction
            )

        M.TargetSelected _ _ ->
            -- This case should not be happened.
            ( m, M.NoAction )


mouseEnd :
    { shiftKey : Bool, spaceKey : Bool }
    -> M.PanePoint
    -> M.PanePoint
    -> M.MakieRecord
    -> ( M.MakieRecord, M.Action )
mouseEnd r lastPoint panePoint ({ camera } as m) =
    let
        cameraMove =
            Vector2d.from lastPoint panePoint |> M.Move

        updateHandle hdl =
            Annotations.updateHandle (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint) hdl

        newAnnotation hRecord =
            { label = hRecord.label
            , notices = hRecord.notices
            , shape = updateHandle hRecord.handle |> Annotations.shapeTo
            }
    in
    case m.target of
        M.NoTarget ->
            case ( m.mode, r.spaceKey ) of
                ( _, True ) ->
                    -- When space key is pressed, app behave like browse mode.
                    ( m, cameraMove )

                ( M.BrowseMode, False ) ->
                    if r.shiftKey then
                        let
                            centerPoint =
                                M.panePoint { x = toFloat m.paneWidth / 2, y = toFloat m.paneHeight / 2 }
                        in
                        case calcAngle centerPoint lastPoint panePoint of
                            Just a ->
                                ( m, M.Rotate centerPoint a )

                            Nothing ->
                                ( m, M.NoAction )

                    else
                        ( m, cameraMove )

                _ ->
                    ( m, M.NoAction )

        M.TargetCreating handleRecord ->
            ( { m | target = M.NoTarget }, M.Add (newAnnotation handleRecord) )

        M.TargetEditing targetId handleRecord ->
            ( { m | target = M.NoTarget }, M.Insert targetId (newAnnotation handleRecord) )

        M.TargetSelected _ _ ->
            -- This case should not be happened.
            ( m, M.NoAction )


getTouchedAnnotation : M.PanePoint -> M.MakieRecord -> Maybe ( String, M.AnnotationRecord )
getTouchedAnnotation panePoint { camera, annotations } =
    let
        tolerance =
            Pixels.pixels 5 |> Quantity.at camera.reductionRate
    in
    Annotations.getTouched
        tolerance
        (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint)
        annotations


getAnnotationHandle : M.PanePoint -> M.MakieRecord -> M.AnnotationRecord -> Maybe M.AnnotationHandle
getAnnotationHandle panePoint { camera } { shape } =
    -- ここからスタート
    let
        tolerance =
            Pixels.pixels 5 |> Quantity.at camera.reductionRate
    in
    Annotations.getHandle
        tolerance
        (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint)
        shape


isShiftKeyPressed : Pointer.Event -> Bool
isShiftKeyPressed =
    .pointer >> .keys >> .shift


offsetPosAsPanePoint : Pointer.Event -> M.PanePoint
offsetPosAsPanePoint =
    .pointer >> .offsetPos >> (\( x, y ) -> M.panePoint { x = x, y = y })


calcAngle : M.PanePoint -> M.PanePoint -> M.PanePoint -> Maybe Angle
calcAngle centerPoint pointA pointB =
    let
        getDirection e =
            e.pointer
                |> .offsetPos
                |> (\( x, y ) -> M.panePoint { x = x, y = y })
                |> Direction2d.from centerPoint
    in
    case ( Direction2d.from centerPoint pointA, Direction2d.from centerPoint pointB ) of
        ( Just directionA, Just directionB ) ->
            Just (Direction2d.angleFrom directionA directionB)

        _ ->
            Nothing
