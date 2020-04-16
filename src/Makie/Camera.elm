module Makie.Camera exposing (..)

import Makie.Internal.Camera as C
import Makie.Internal.Makie as M exposing (Camera)


type alias Camera =
    Camera


center : Camera -> { x : Float, y : Float }
center (M.Camera r) =
    -- TODO
    { x = 0, y = 0 }
