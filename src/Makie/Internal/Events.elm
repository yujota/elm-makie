module Makie.Internal.Events exposing (handlePointerEvent)

import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Makie as M


handlePointerEvent : M.PointerEvent -> M.EventStatusRecord -> ( M.EventStatusRecord, M.Action )
handlePointerEvent ev r =
    case r.mode of
        M.ZeroPointer ->
            zeroPointAction ev r

        M.OnePointer p ->
            onePointAction ev r p

        _ ->
            ( r, M.NoAction )



-- Helper functions


zeroPointAction : M.PointerEvent -> M.EventStatusRecord -> ( M.EventStatusRecord, M.Action )
zeroPointAction event _ =
    case event of
        M.OnDown e ->
            ( { mode = M.OnePointer e }, M.NoAction )

        _ ->
            ( { mode = M.ZeroPointer }, M.NoAction )


onePointAction : M.PointerEvent -> M.EventStatusRecord -> Pointer.Event -> ( M.EventStatusRecord, M.Action )
onePointAction event s p =
    case event of
        M.OnDown _ ->
            ( s, M.NoAction )

        M.OnMove e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.OnePointer e }, getDiff p e |> M.paneVector |> M.Move |> M.CameraActionVariant )

            else
                ( s, M.NoAction )

        M.OnUp e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.ZeroPointer }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnCancel e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.ZeroPointer }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnOut e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.ZeroPointer }, M.NoAction )

            else
                ( s, M.NoAction )

        M.OnLeave e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = M.ZeroPointer }, M.NoAction )

            else
                ( s, M.NoAction )


getDiff : Pointer.Event -> Pointer.Event -> { dx : Float, dy : Float }
getDiff pointerA pointerB =
    let
        offsetPos e =
            .offsetPos e.pointer
    in
    ( pointerA, pointerB )
        |> Tuple.mapBoth offsetPos offsetPos
        |> (\( a, b ) -> { dx = Tuple.first b - Tuple.first a, dy = Tuple.second b - Tuple.second a })
