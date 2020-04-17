module Makie.Canvas exposing (render, renderables, textures)

import Canvas exposing (Renderable)
import Canvas.Texture exposing (Source)
import Makie.Internal.Makie as M exposing (Event, Makie)


textures : Makie -> (Event -> msg) -> List (Source msg)
textures (M.Makie m) lifter =
    case m.contents of
        M.SingleImageCanvasContents c ->
            [ Canvas.Texture.loadFromImageUrl c.src (M.SingleImageCanvasTextureLoaded >> lifter) ]


renderables : Makie -> List Renderable
renderables (M.Makie m) =
    case m.contents of
        M.SingleImageCanvasContents c ->
            c.renderables


render : Makie -> Makie
render m =
    m
