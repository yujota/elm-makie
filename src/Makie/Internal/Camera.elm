module Makie.Internal.Camera exposing (apply, camera)

import Angle exposing (Angle)
import Direction2d
import Frame2d
import Makie.Internal.Makie as M exposing (Camera)
import Point2d
import Quantity
import Vector2d


camera : { imageWidth : Int, imageHeight : Int, paneWidth : Int, paneHeight : Int } -> M.CameraRecord
camera r =
    { imageFrame = Frame2d.atOrigin, reductionRate = M.reductionRate 1, angle = Angle.degrees 0 }


apply : M.CameraAction -> M.CameraRecord -> M.CameraRecord
apply act r =
    case act of
        M.Move paneVector ->
            applyMove paneVector r

        M.Zoom panePoint newReductionRate ->
            applyZoom panePoint newReductionRate r

        M.Rotate panePoint angle ->
            applyRotate panePoint angle r



-- Helper functions


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


applyRotate : M.PanePoint -> Angle -> M.CameraRecord -> M.CameraRecord
applyRotate panePoint angle r =
    { r | imageFrame = Frame2d.rotateAround panePoint angle r.imageFrame, angle = Quantity.plus angle r.angle }
