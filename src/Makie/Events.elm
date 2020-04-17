module Makie.Events exposing (onPointerOperations)

import Html exposing (Attribute)
import Html.Events.Extra.Pointer as Pointer
import Makie.Internal.Makie as M exposing (Action, EventMode(..), EventStatus)


onPointerOperations : List (Attribute M.Event)
onPointerOperations =
    [ Pointer.onDown (M.OnDown >> M.PointerEventVariant)
    , Pointer.onMove (M.OnMove >> M.PointerEventVariant)
    , Pointer.onUp (M.OnUp >> M.PointerEventVariant)
    , Pointer.onCancel (M.OnCancel >> M.PointerEventVariant)
    , Pointer.onOut (M.OnOut >> M.PointerEventVariant)
    , Pointer.onLeave (M.OnLeave >> M.PointerEventVariant)
    ]
