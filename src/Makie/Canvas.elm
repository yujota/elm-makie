module Makie.Canvas exposing (render, renderables, textures)

import Canvas exposing (Renderable)
import Canvas.Texture exposing (Source)
import Makie.Events exposing (PointerEventSpec)
import Makie.Internal.NewMakie exposing (Makie)


textures : Makie -> (PointerEventSpec -> msg) -> List (Source msg)
textures m lifter =
    -- TODO
    []


renderables : Makie -> List Renderable
renderables m =
    -- TODO
    []


render : Makie -> Makie
render m =
    m
