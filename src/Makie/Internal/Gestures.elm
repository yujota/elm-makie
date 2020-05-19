module Makie.Internal.Gestures exposing (clear, gestureModel, update)

import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Makie as M
import Set exposing (Set)


gestureModel : M.GestureModel
gestureModel =
    { status = M.HandlingNothing
    , isSpaceKeyPressed = False
    , isPenDeviceDetected = False
    }


update : M.PointerEvent -> M.GestureModel -> ( M.GestureModel, M.Gesture )
update event model =
    case ( event, model.status ) of
        ( M.OnDown e, M.HandlingNothing ) ->
            let
                panePoint =
                    offsetPosAsPanePoint e
            in
            case e.pointerType of
                Pointer.MouseType ->
                    ( { model
                        | status =
                            M.HandlingMouse
                                { pointerId = e.pointerId
                                , lastPosition = panePoint
                                , shiftKey = isShiftKeyPressed e
                                , spaceKey = model.isSpaceKeyPressed
                                }
                      }
                    , M.MouseStart { shiftKey = isShiftKeyPressed e, spaceKey = model.isSpaceKeyPressed } panePoint
                    )

                Pointer.TouchType ->
                    -- TODO: Implement Multi touch detection
                    ( { model
                        | status =
                            M.HandlingSingleTouch
                                { pointerId = e.pointerId
                                , lastPosition = panePoint
                                , smartStylus = model.isPenDeviceDetected
                                }
                      }
                    , M.SingleTouchStart { smartStylus = model.isPenDeviceDetected } panePoint
                    )

                Pointer.PenType ->
                    ( { model
                        | status = M.HandlingPen { pointerId = e.pointerId, lastPosition = panePoint }
                        , isPenDeviceDetected = True
                      }
                    , M.PenStart panePoint
                    )

        ( M.OnDown _, _ ) ->
            ( model, M.NoGesture )

        ( M.OnMove e, M.HandlingMouse ({ pointerId, lastPosition, shiftKey, spaceKey } as r) ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.MouseType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingMouse { r | lastPosition = panePoint } }
                    , M.MouseOnGoing { shiftKey = shiftKey, spaceKey = spaceKey } lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnMove e, M.HandlingPen ({ pointerId, lastPosition } as r) ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.PenType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingPen { r | lastPosition = panePoint } }
                    , M.PenOnGoing lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnMove e, M.HandlingSingleTouch ({ pointerId, lastPosition, smartStylus } as r) ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.TouchType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingSingleTouch { r | lastPosition = panePoint } }
                    , M.SingleTouchOnGoing { smartStylus = smartStylus } lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnMove _, _ ) ->
            ( model, M.NoGesture )

        ( M.OnUp _, M.HandlingNothing ) ->
            ( model, M.NoGesture )

        ( M.OnUp e, M.Cooling pointerIds ) ->
            let
                newPointerIds =
                    Set.remove e.pointerId pointerIds
            in
            if Set.isEmpty newPointerIds then
                ( { model | status = M.HandlingNothing }, M.NoGesture )

            else
                ( { model | status = M.Cooling newPointerIds }, M.NoGesture )

        ( M.OnUp e, M.HandlingMouse { pointerId, lastPosition, shiftKey, spaceKey } ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.MouseType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingNothing }
                    , M.MouseEnd { shiftKey = shiftKey, spaceKey = spaceKey } lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnUp e, M.HandlingPen { pointerId, lastPosition } ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.PenType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingNothing }
                    , M.PenEnd lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnUp e, M.HandlingSingleTouch { pointerId, lastPosition, smartStylus } ) ->
            case ( e.pointerType, e.pointerId == pointerId ) of
                ( Pointer.TouchType, True ) ->
                    let
                        panePoint =
                            offsetPosAsPanePoint e
                    in
                    ( { model | status = M.HandlingNothing }
                    , M.SingleTouchEnd { smartStylus = smartStylus } lastPosition panePoint
                    )

                _ ->
                    ( model, M.NoGesture )

        ( M.OnCancel e, _ ) ->
            update (M.OnUp e) model

        ( M.OnOut e, _ ) ->
            update (M.OnUp e) model

        ( M.OnLeave e, _ ) ->
            update (M.OnUp e) model


clear : M.GestureModel -> M.GestureModel
clear model =
    case model.status of
        M.HandlingMouse { pointerId } ->
            { model | status = M.Cooling (Set.singleton pointerId) }

        M.HandlingPen { pointerId } ->
            { model | status = M.Cooling (Set.singleton pointerId) }

        M.HandlingSingleTouch { pointerId } ->
            { model | status = M.Cooling (Set.singleton pointerId) }

        _ ->
            model



-- Helper functions


isShiftKeyPressed : Pointer.Event -> Bool
isShiftKeyPressed =
    .pointer >> .keys >> .shift


offsetPosAsPanePoint : Pointer.Event -> M.PanePoint
offsetPosAsPanePoint =
    .pointer >> .offsetPos >> (\( x, y ) -> M.panePoint { x = x, y = y })
