module Makie.Internal.Camera exposing (camera, move, rotate, zoom)

import Angle exposing (Angle)
import Frame2d
import Makie.Internal.Makie as M exposing (Camera)
import Point2d
import Quantity
import Vector2d


camera : { imageWidth : Int, imageHeight : Int, paneWidth : Int, paneHeight : Int } -> M.CameraRecord
camera r =
    { imageFrame = Frame2d.atOrigin, reductionRate = M.reductionRate 1, angle = Angle.degrees 0 }


move : M.PaneVector -> M.CameraRecord -> M.CameraRecord
move paneVector r =
    { r | imageFrame = Frame2d.translateBy paneVector r.imageFrame }


zoom : M.PanePoint -> M.ReductionRate -> M.CameraRecord -> M.CameraRecord
zoom panePoint newReductionRate r =
    let
        ratio =
            -- Quantity.ratio newReductionRate r.reductionRate
            Quantity.ratio r.reductionRate newReductionRate

        zoomPointToImageOrigin =
            Vector2d.from panePoint (M.toPanePoint r.reductionRate r.imageFrame Point2d.origin)

        newImageOrigin =
            Point2d.translateBy (Vector2d.scaleBy ratio zoomPointToImageOrigin) panePoint
    in
    { r | imageFrame = Frame2d.moveTo newImageOrigin r.imageFrame, reductionRate = newReductionRate }


rotate : M.PanePoint -> Angle -> M.CameraRecord -> M.CameraRecord
rotate panePoint angle r =
    { r | imageFrame = Frame2d.rotateAround panePoint angle r.imageFrame, angle = Quantity.plus angle r.angle }
