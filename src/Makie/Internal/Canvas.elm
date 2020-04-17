module Makie.Internal.Canvas exposing
    ( handleSingleImageCanvasTextureLoaded
    , renderSingleImageCanvas
    , singleImageCanvasContents
    )

import Canvas
import Canvas.Settings.Advanced as CAdvanced
import Canvas.Texture exposing (Texture)
import Makie.Internal.Makie as M
import Pixels
import Point2d



-- Single Image


singleImageCanvasContents : { src : String } -> M.Contents
singleImageCanvasContents r =
    M.SingleImageCanvasContents
        { texture = Nothing
        , src = r.src
        , renderables = []
        , isRenderingRequested = True
        }


handleSingleImageCanvasTextureLoaded :
    Maybe Texture
    -> M.SingleImageCanvasContentsRecord
    -> M.SingleImageCanvasContentsRecord
handleSingleImageCanvasTextureLoaded maybeTexture r =
    case maybeTexture of
        Just txt ->
            { r | texture = Just txt, isRenderingRequested = True }

        Nothing ->
            r


renderSingleImageCanvas :
    { paneWidth : Int, paneHeight : Int, imageWidth : Int, imageHeight : Int, camera : M.CameraRecord }
    -> M.SingleImageCanvasContentsRecord
    -> M.SingleImageCanvasContentsRecord
renderSingleImageCanvas { camera } c =
    case c.texture of
        Just t ->
            let
                ( x, y ) =
                    Point2d.origin
                        |> M.toPanePoint camera.reductionRate camera.imageFrame
                        |> Point2d.toTuple Pixels.inPixels
            in
            Canvas.texture [ CAdvanced.transform [ CAdvanced.translate x y ] ] ( 0, 0 ) t
                |> (\rnd -> { c | renderables = [ rnd ] })

        Nothing ->
            c
