module Makie.Internal.Camera exposing (apply, camera)

import Frame2d
import Makie.Internal.Makie as M exposing (Camera)


camera : { imageWidth : Int, imageHeight : Int, paneWidth : Int, paneHeight : Int } -> M.CameraRecord
camera r =
    { imageFrame = Frame2d.atOrigin, reductionRate = M.reductionRate 1 }


apply : M.CameraAction -> M.CameraRecord -> M.CameraRecord
apply act r =
    case act of
        M.Move paneVector ->
            applyMove paneVector r

        _ ->
            r


applyMove : M.PaneVector -> M.CameraRecord -> M.CameraRecord
applyMove paneVector r =
    { r | imageFrame = Frame2d.translateBy paneVector r.imageFrame }
