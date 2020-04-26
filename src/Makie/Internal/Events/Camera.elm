module Makie.Internal.Events.Camera exposing (interpretPointerEvents, interpretWheelEvents)

import Angle exposing (Angle)
import Direction2d
import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Makie as M
import Quantity


interpretPointerEvents : M.PointerEvent -> M.MakieRecord -> ( M.MakieRecord, M.Action )
interpretPointerEvents ev ({ eventStatus, paneWidth, paneHeight } as m) =
    let
        wrap =
            Tuple.mapFirst (\eStatus -> { m | eventStatus = eStatus })
    in
    case eventStatus.mode of
        M.NoGesture ->
            noGesture ev eventStatus |> wrap

        M.SingleTouchGesture p ->
            singleTouch ev eventStatus p |> wrap

        M.RotateByCenterGesture p ->
            rotateByCenter { paneWidth = paneWidth, paneHeight = paneHeight } ev eventStatus p |> wrap

        _ ->
            ( m, M.NoAction )


interpretWheelEvents : M.WheelEvent -> M.MakieRecord -> ( M.MakieRecord, M.Action )
interpretWheelEvents (M.OnWheel ev) ({ camera, eventStatus } as m) =
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



-- Helper functions


noGesture : M.PointerEvent -> M.EventStatus -> ( M.EventStatus, M.Action )
noGesture event _ =
    case event of
        M.OnDown e ->
            let
                isShiftKeyPressed evt =
                    evt |> .pointer |> .keys |> .shift
            in
            case isShiftKeyPressed e of
                True ->
                    ( { mode = M.RotateByCenterGesture e }, M.NoAction )

                False ->
                    ( { mode = M.SingleTouchGesture e }, M.NoAction )

        _ ->
            ( { mode = M.NoGesture }, M.NoAction )


singleTouch : M.PointerEvent -> M.EventStatus -> Pointer.Event -> ( M.EventStatus, M.Action )
singleTouch event s p =
    case event of
        M.OnDown _ ->
            ( s, M.NoAction )

        M.OnMove e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.SingleTouchGesture e }, calcDiff p e |> M.paneVector |> M.Move |> M.CameraActionVariant )

            else
                ( s, M.NoAction )

        M.OnUp e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.NoGesture }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnCancel e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.NoGesture }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnOut e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.NoGesture }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnLeave e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.NoGesture }, M.NoAction )

            else
                ( s, M.NoAction )


rotateByCenter :
    { paneWidth : Int, paneHeight : Int }
    -> M.PointerEvent
    -> M.EventStatus
    -> Pointer.Event
    -> ( M.EventStatus, M.Action )
rotateByCenter { paneWidth, paneHeight } event s p =
    case event of
        M.OnMove e ->
            if e.pointerId == p.pointerId then
                let
                    centerPoint =
                        M.panePoint { x = toFloat paneWidth / 2, y = toFloat paneHeight / 2 }
                in
                ( { s | mode = M.RotateByCenterGesture e }
                , calcAngle centerPoint p e
                    |> Maybe.map (\a -> M.Rotate centerPoint a |> M.CameraActionVariant)
                    |> Maybe.withDefault M.NoAction
                )

            else
                ( s, M.NoAction )

        _ ->
            singleTouch event s p


calcDiff : Pointer.Event -> Pointer.Event -> { dx : Float, dy : Float }
calcDiff pointerA pointerB =
    let
        offsetPos e =
            .offsetPos e.pointer
    in
    ( pointerA, pointerB )
        |> Tuple.mapBoth offsetPos offsetPos
        |> (\( a, b ) -> { dx = Tuple.first b - Tuple.first a, dy = Tuple.second b - Tuple.second a })


calcAngle : M.PanePoint -> Pointer.Event -> Pointer.Event -> Maybe Angle
calcAngle centerPoint pointerA pointerB =
    let
        getDirection e =
            e.pointer
                |> .offsetPos
                |> (\( x, y ) -> M.panePoint { x = x, y = y })
                |> Direction2d.from centerPoint
    in
    case ( getDirection pointerA, getDirection pointerB ) of
        ( Just directionA, Just directionB ) ->
            Just (Direction2d.angleFrom directionA directionB)

        _ ->
            Nothing
