module Makie.Internal.Gestures exposing (toGesture)

import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Makie as M


toGesture : M.PointerEvent -> M.Gesture -> M.Gesture
toGesture event gesture =
    case gesture.status of
        M.NoGesture ->
            noGesture event gesture

        M.GestureDetectionSuspended _ ->
            -- TODO
            gesture

        M.GestureStart gestureType ->
            gestureContinues event gestureType gesture

        M.GestureOngoing gestureType ->
            gestureContinues event gestureType gesture

        M.GestureEnd gestureType ->
            -- TODO: Consider the case that : double tap -> release one touch -> touch again
            noGesture event gesture



-- Helper functions


noGesture : M.PointerEvent -> M.Gesture -> M.Gesture
noGesture event gesture =
    case event of
        M.OnDown e ->
            case e.pointerType of
                Pointer.MouseType ->
                    if isShiftKeyPressed e then
                        { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSiftGesture |> M.GestureStart }

                    else if gesture.isSpaceKeyPressed then
                        { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSpaceGesture |> M.GestureStart }

                    else
                        { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveGesture |> M.GestureStart }

                Pointer.TouchType ->
                    -- TODO: Implement later
                    gesture

                Pointer.PenType ->
                    { gesture
                        | status = offsetPosAsPanePoint e |> M.PenGesture |> M.GestureStart
                        , penDeviceDetected = True
                    }

        _ ->
            gesture


gestureContinues : M.PointerEvent -> M.GestureType -> M.Gesture -> M.Gesture
gestureContinues event gestureType gesture =
    case event of
        M.OnDown _ ->
            gesture

        M.OnMove e ->
            case gestureType of
                M.MouseMoveGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveGesture |> M.GestureOngoing }

                M.MouseMoveWithSiftGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSiftGesture |> M.GestureOngoing }

                M.MouseMoveWithSpaceGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSpaceGesture |> M.GestureOngoing }

                M.PenGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.PenGesture |> M.GestureOngoing }

                M.SingleTouchGesture pointerId _ ->
                    if e.pointerId == pointerId then
                        { gesture
                            | status =
                                offsetPosAsPanePoint e
                                    |> M.SingleTouchGesture pointerId
                                    |> M.GestureOngoing
                        }

                    else
                        gesture

                M.DoubleTouchGesture ->
                    -- TODO
                    gesture

                M.PinchCloseGesture ->
                    -- TODO
                    gesture

                M.PinchCloseAndRotateGesture ->
                    -- TODO
                    gesture

        M.OnUp e ->
            case gestureType of
                M.MouseMoveGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveGesture |> M.GestureEnd }

                M.MouseMoveWithSiftGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSiftGesture |> M.GestureEnd }

                M.MouseMoveWithSpaceGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.MouseMoveWithSpaceGesture |> M.GestureEnd }

                M.PenGesture _ ->
                    { gesture | status = offsetPosAsPanePoint e |> M.PenGesture |> M.GestureEnd }

                M.SingleTouchGesture _ _ ->
                    -- TODO
                    gesture

                M.DoubleTouchGesture ->
                    -- TODO
                    gesture

                M.PinchCloseGesture ->
                    -- TODO
                    gesture

                M.PinchCloseAndRotateGesture ->
                    -- TODO
                    gesture

        M.OnCancel e ->
            gestureContinues (M.OnUp e) gestureType gesture

        M.OnOut e ->
            gestureContinues (M.OnUp e) gestureType gesture

        M.OnLeave e ->
            gestureContinues (M.OnUp e) gestureType gesture


isShiftKeyPressed : Pointer.Event -> Bool
isShiftKeyPressed =
    .pointer >> .keys >> .shift


offsetPosAsPanePoint : Pointer.Event -> M.PanePoint
offsetPosAsPanePoint =
    .pointer >> .offsetPos >> (\( x, y ) -> M.panePoint { x = x, y = y })
