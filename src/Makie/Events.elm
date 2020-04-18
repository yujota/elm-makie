module Makie.Events exposing (onPointerEvents, onWheelEvents)

import Html exposing (Attribute)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Makie.Internal.Makie as M exposing (Action, EventMode(..), EventStatus)


onPointerEvents : List (Attribute M.Event)
onPointerEvents =
    [ Pointer.onDown (M.OnDown >> M.PointerEventVariant)
    , Pointer.onMove (M.OnMove >> M.PointerEventVariant)
    , Pointer.onUp (M.OnUp >> M.PointerEventVariant)
    , Pointer.onCancel (M.OnCancel >> M.PointerEventVariant)
    , Pointer.onOut (M.OnOut >> M.PointerEventVariant)
    , Pointer.onLeave (M.OnLeave >> M.PointerEventVariant)
    ]


onWheelEvents : List (Attribute M.Event)
onWheelEvents =
    [ Wheel.onWheel (M.OnWheel >> M.WheelEventVariant)
    ]
