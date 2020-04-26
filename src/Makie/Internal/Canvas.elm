module Makie.Internal.Canvas exposing
    ( handleSingleImageCanvasTextureLoaded
    , renderSingleImageCanvas
    , singleImageCanvasContents
    )

import Angle
import Canvas
import Canvas.Settings.Advanced as CAdvanced
import Canvas.Texture exposing (Texture)
import Makie.Internal.Makie as M
import Pixels
import Point2d
import Quantity



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

                degrees =
                    camera.angle |> Angle.inRadians

                scale =
                    Quantity.ratio camera.reductionRate (M.reductionRate 1)
            in
            Canvas.texture
                [ CAdvanced.transform
                    [ CAdvanced.translate x y
                    , CAdvanced.scale scale scale
                    , CAdvanced.rotate degrees
                    ]
                ]
                ( 0, 0 )
                t
                |> (\rnd -> { c | renderables = [ rnd ] })

        Nothing ->
            c



-- Annotations


renderAnnotation : M.Category -> M.AnnotationRecord -> List Canvas.Renderable
renderAnnotation category { label, notices, shape } =
    case shape of
        M.Point pointShape ->
            []

        M.Rectangle rectangleShape ->
            []

        M.Polygon polygonShape ->
            []
