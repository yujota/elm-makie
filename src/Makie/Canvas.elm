module Makie.Canvas exposing (render, renderables, textures)

import Canvas exposing (Renderable)
import Canvas.Texture exposing (Source)
import Makie.Internal.Makie as M exposing (Event, Makie)


textures : Makie -> (Event -> msg) -> List (Source msg)
textures (M.Makie { display }) lifter =
    [ Canvas.Texture.loadFromImageUrl display.src (M.SingleImageCanvasTextureLoaded >> lifter) ]


renderables : Makie -> List Renderable
renderables (M.Makie { display }) =
    display.background ++ display.images ++ display.annotations ++ display.editing


render : Makie -> Makie
render m =
    m
