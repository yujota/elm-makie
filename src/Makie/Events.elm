module Makie.Events exposing (PointerEventSpec, initialStatus, none, onPointerOperations, toAction)

import Html exposing (Attribute)
import Html.Events.Extra.Pointer as Pointer
import Makie.Actions as Actions exposing (Action)
import Makie.Actions.Camera as Camera
import Makie.Internal.Makie as M exposing (EventMode(..), EventStatus)


type PointerEventSpec
    = OnDown Pointer.Event
    | OnMove Pointer.Event
    | OnUp Pointer.Event
    | OnCancel Pointer.Event
    | OnOut Pointer.Event
    | OnLeave Pointer.Event


none : PointerEventSpec
none =
    Debug.todo "todo"


onPointerOperations : List (Attribute PointerEventSpec)
onPointerOperations =
    [ Pointer.onDown OnDown
    , Pointer.onMove OnMove
    , Pointer.onUp OnUp
    , Pointer.onCancel OnCancel
    , Pointer.onOut OnOut
    , Pointer.onLeave OnLeave
    ]


toAction : PointerEventSpec -> EventStatus -> ( EventStatus, Action )
toAction event (M.EventStatus r) =
    Tuple.mapFirst M.EventStatus <|
        case r.mode of
            ZeroPointer ->
                zeroPointAction event r

            OnePointer p ->
                onePointAction event r p

            _ ->
                ( r, Actions.none )



-- Helper functions


zeroPointAction : PointerEventSpec -> M.EventStatusRecord -> ( M.EventStatusRecord, Action )
zeroPointAction event _ =
    case event of
        OnDown e ->
            ( { mode = OnePointer e }, Actions.none )

        _ ->
            ( { mode = ZeroPointer }, Actions.none )


onePointAction : PointerEventSpec -> M.EventStatusRecord -> Pointer.Event -> ( M.EventStatusRecord, Action )
onePointAction event s p =
    case event of
        OnDown _ ->
            ( s, Actions.none )

        OnMove e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = OnePointer e }, Camera.move (getDiff p e) )

            else
                ( s, Actions.none )

        OnUp e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = ZeroPointer }, Actions.none )

            else
                ( s, Actions.none )

        OnCancel e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = ZeroPointer }, Actions.none )

            else
                ( s, Actions.none )

        OnOut e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = ZeroPointer }, Actions.none )

            else
                ( s, Actions.none )

        OnLeave e ->
            if e.pointerId == p.pointerId then
                ( { s | mode = ZeroPointer }, Actions.none )

            else
                ( s, Actions.none )


getDiff : Pointer.Event -> Pointer.Event -> { dx : Float, dy : Float }
getDiff pointerA pointerB =
    let
        offsetPos e =
            .offsetPos e.pointer
    in
    ( pointerA, pointerB )
        |> Tuple.mapBoth offsetPos offsetPos
        |> (\( a, b ) -> { dx = Tuple.first b - Tuple.first a, dy = Tuple.second b - Tuple.second a })
