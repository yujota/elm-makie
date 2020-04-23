module Makie.Internal.Annotations.Rectangle exposing (getHandle, isTouched)

import Array
import Makie.Internal.Makie as M
import Point2d
import Rectangle2d exposing (Rectangle2d)


isTouched : M.ImagePixels -> M.RectangleAnnotationRecord -> M.ImagePoint -> Bool
isTouched _ { rectangle } point =
    Rectangle2d.contains point rectangle


getHandle : M.ImagePixels -> M.RectangleAnnotationRecord -> M.ImagePoint -> Maybe M.RectangleAnnotationHandle
getHandle tolerance r point =
    let
        loop handlers =
            case handlers of
                g :: rest ->
                    case g tolerance r point of
                        Just h ->
                            Just h

                        Nothing ->
                            loop rest

                [] ->
                    Nothing
    in
    loop [ getHandleCorner, getHandleMove ]


create : M.ImagePoint -> M.RectangleAnnotationHandle
create point =
    M.RectangleCornerHandle { anchor = point, control = point }


updateControlPoint : M.ImagePoint -> M.RectangleAnnotationHandle -> M.RectangleAnnotationHandle
updateControlPoint point hdl =
    case hdl of
        M.RectangleMoveHandle r ->
            M.RectangleMoveHandle { r | control = point }

        M.RectangleCornerHandle r ->
            M.RectangleCornerHandle { r | control = point }


finish : M.RectangleAnnotationHandle -> Result String M.RectangleAnnotationRecord
finish hdl =
    Err "Error"



-- Helpers


getHandleCorner : M.ImagePixels -> M.RectangleAnnotationRecord -> M.ImagePoint -> Maybe M.RectangleAnnotationHandle
getHandleCorner tolerance r point =
    let
        vertices =
            Rectangle2d.vertices r.rectangle |> Array.fromList

        getHdl i =
            case ( Array.get i vertices, Array.get ((i + 2) // 4) vertices ) of
                ( Just p, Just anc ) ->
                    if Point2d.equalWithin tolerance p point then
                        Just (M.RectangleCornerHandle { anchor = anc, control = p })

                    else
                        Nothing

                _ ->
                    Nothing

        loop i =
            case ( getHdl i, i <= 3 ) of
                ( Just h, _ ) ->
                    Just h

                ( Nothing, True ) ->
                    loop (i + 1)

                ( Nothing, False ) ->
                    Nothing
    in
    loop 0


getHandleMove : M.ImagePixels -> M.RectangleAnnotationRecord -> M.ImagePoint -> Maybe M.RectangleAnnotationHandle
getHandleMove tolerance r point =
    if Rectangle2d.contains point r.rectangle then
        Just (M.RectangleMoveHandle { start = point, control = point, original = r.rectangle })

    else
        Nothing
