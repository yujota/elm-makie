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
            ( m, M.Zoom zoomPoint newReductionRate |> M.CameraActionVariant )

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
interpretPointerEvents event ({ gesture, paneWidth, paneHeight } as m) =
    let
        newGesture =
            Gestures.toGesture event gesture
    in
    Tuple.mapFirst (\r -> { r | gesture = newGesture }) <|
        case ( gesture.status, newGesture.status ) of
            ( _, M.GestureStart (M.MouseMoveGesture p) ) ->
                -- TODO: 4/26 はここから実装
                ( m, M.NoAction )

            _ ->
                ( m, M.NoAction )


noGesture : M.PointerEvent -> M.MakieRecord -> ( M.MakieRecord, M.Action )
noGesture event m =
    (\r -> ( r, M.NoAction )) <|
        case event of
            M.OnDown e ->
                case e.pointerType of
                    Pointer.MouseType ->
                        if isShiftKeyPressed e then
                            { m | gesture = M.MouseCameraRotateByCenterGesture (offsetPosAsPanePoint e) }

                        else
                            noGestureMouseOnDown (offsetPosAsPanePoint e) m

                    Pointer.TouchType ->
                        -- TODO: Implement later
                        m

                    Pointer.PenType ->
                        { m | gesture = M.PenGesture e.pointerId <| offsetPosAsPanePoint e, smartStylus = True }

            _ ->
                m


noGestureMouseOnDown : M.PanePoint -> M.MakieRecord -> M.MakieRecord
noGestureMouseOnDown panePoint ({ camera } as m) =
    case m.target of
        M.NoTarget ->
            case getTouchedAnnotation panePoint m of
                Just ( key, ant ) ->
                    -- In case, selecting annotations
                    { m | target = M.TargetSelected key ant }

                Nothing ->
                    case m.mode of
                        M.BrowseMode ->
                            { m | gesture = M.MouseCameraMoveGesture panePoint }

                        M.PointMode ->
                            { m
                                | gesture = M.MouseAnnotationHandleGesture panePoint
                                , target =
                                    M.fromPanePoint camera.reductionRate camera.imageFrame panePoint
                                        |> Annotations.startPoint
                                        |> M.TargetCreating
                            }

                        M.RectangleMode ->
                            { m
                                | gesture = M.MouseAnnotationHandleGesture panePoint
                                , target =
                                    M.fromPanePoint camera.reductionRate camera.imageFrame panePoint
                                        |> Annotations.startRectangle
                                        |> M.TargetCreating
                            }

                        M.PolygonMode ->
                            -- TODO
                            m

                        M.SelectionRectangleMode ->
                            -- TODO
                            m

        M.TargetCreating _ ->
            -- This case should not be happened.
            { m | target = M.NoTarget }

        M.TargetSelected targetId targetAnnotation ->
            case Annotations.getHandle of
                _ ->
                    m

        M.TargetEditing targetId targetAnnotation ->
            -- This case should not be happened.
            { m | target = M.NoTarget }



{-
   case (getTouchedAnnotation panePoint m, m.target) of
       (Just (key, ant), M.NoTarget) ->
       (Just (key, ant), M.TargetSelected targetKey targetAnnotaton) ->
-}


getTouchedAnnotation : M.PanePoint -> M.MakieRecord -> Maybe ( String, M.AnnotationRecord )
getTouchedAnnotation panePoint ({ camera } as m) =
    let
        tolerance =
            Pixels.pixels 5 |> Quantity.at camera.reductionRate
    in
    Annotations.getTouched
        tolerance
        (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint)
        m.annotations


getAnnotationHandle : M.PanePoint -> M.MakieRecord -> Maybe ( String, M.AnnotationRecord )
getAnnotationHandle panePoint ({ camera } as m) =
    -- ここからスタート
    let
        tolerance =
            Pixels.pixels 5 |> Quantity.at camera.reductionRate
    in
    Annotations.getTouched
        tolerance
        (M.fromPanePoint camera.reductionRate camera.imageFrame panePoint)
        m.annotations


isShiftKeyPressed : Pointer.Event -> Bool
isShiftKeyPressed =
    .pointer >> .keys >> .shift


offsetPosAsPanePoint : Pointer.Event -> M.PanePoint
offsetPosAsPanePoint =
    .pointer >> .offsetPos >> (\( x, y ) -> M.panePoint { x = x, y = y })
