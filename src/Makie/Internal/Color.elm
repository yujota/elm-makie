module Makie.Internal.Color exposing (..)

import Color exposing (Color)


mapSaturation : (Float -> Float) -> Color -> Color
mapSaturation f color =
    color
        |> Color.toHsla
        |> (\r -> { r | saturation = f r.saturation })
        |> Color.fromHsla


mapAlpha : (Float -> Float) -> Color -> Color
mapAlpha f color =
    color
        |> Color.toHsla
        |> (\r -> { r | alpha = f r.alpha })
        |> Color.fromHsla
