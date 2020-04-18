module Makie.Internal.Camera exposing (apply, camera)

import Frame2d
import Makie.Internal.Makie as M exposing (Camera)
import Point2d
import Quantity
import Vector2d


camera : { imageWidth : Int, imageHeight : Int, paneWidth : Int, paneHeight : Int } -> M.CameraRecord
camera r =
    { imageFrame = Frame2d.atOrigin, reductionRate = M.reductionRate 1 }


apply : M.CameraAction -> M.CameraRecord -> M.CameraRecord
apply act r =
    case act of
        M.Move paneVector ->
            applyMove paneVector r

        M.Zoom panePoint newReductionRate ->
            applyZoom panePoint newReductionRate r

        _ ->
            r


applyMove : M.PaneVector -> M.CameraRecord -> M.CameraRecord
applyMove paneVector r =
    { r | imageFrame = Frame2d.translateBy paneVector r.imageFrame }


applyZoom : M.PanePoint -> M.ReductionRate -> M.CameraRecord -> M.CameraRecord
applyZoom panePoint newReductionRate r =
    let
        ratio =
            Quantity.ratio newReductionRate r.reductionRate

        zoomPointToImageOrigin =
            Vector2d.from panePoint (M.toPanePoint r.reductionRate r.imageFrame Point2d.origin)

        newImageOrigin =
            Point2d.translateBy (Vector2d.scaleBy ratio zoomPointToImageOrigin) panePoint
    in
    { r | imageFrame = Frame2d.moveTo newImageOrigin r.imageFrame, reductionRate = newReductionRate }
