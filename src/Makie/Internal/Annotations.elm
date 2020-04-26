module Makie.Internal.Annotations exposing (getHandle, getTouched, isTouched, startPoint, startRectangle)

import Array
import BoundingBox2d
import Makie.Internal.Makie as M
import Makie.Internal.ObjectContainer as OC
import Pixels
import Point2d
import Polygon2d
import Quantity exposing (Quantity, Squared)
import Rectangle2d


getTouched :
    M.ImagePixels
    -> M.ImagePoint
    -> M.ObjectContainer M.AnnotationRecord
    -> Maybe ( String, M.AnnotationRecord )
getTouched tolerance imagePoint container =
    BoundingBox2d.withDimensions ( tolerance, tolerance ) imagePoint
        |> (\b -> OC.touched (.shape >> isTouched tolerance imagePoint) b container)
        |> List.sortBy (Tuple.second >> .shape >> roughSize)
        |> List.head


getHandle : M.ImagePixels -> M.ImagePoint -> M.Shape -> Maybe M.AnnotationHandle
getHandle tolerance imagePoint shape =
    case shape of
        M.Point pointShape ->
            if Point2d.equalWithin tolerance imagePoint pointShape.point then
                M.PointMove pointShape { start = imagePoint, control = imagePoint } |> Just

            else
                Nothing

        M.Rectangle rectangleShape ->
            getRectangleHandle tolerance imagePoint rectangleShape

        M.Polygon polygonShape ->
            -- TODO
            Nothing


isTouched : M.ImagePixels -> M.ImagePoint -> M.Shape -> Bool
isTouched tolerance imagePoint shape =
    case shape of
        M.Point pointShape ->
            Point2d.equalWithin tolerance imagePoint pointShape.point

        M.Rectangle rectangleShape ->
            Rectangle2d.contains imagePoint rectangleShape.rectangle

        M.Polygon polygonShape ->
            Polygon2d.contains imagePoint polygonShape.polygon


startPoint : M.ImagePoint -> M.AnnotationHandle
startPoint imagePoint =
    M.PointMove { point = imagePoint } { start = imagePoint, control = imagePoint }


startRectangle : M.ImagePoint -> M.AnnotationHandle
startRectangle imagePoint =
    M.RectangleEditCorner { oppositeCorner = imagePoint, control = imagePoint }



-- Helpers


roughSize : M.Shape -> Float
roughSize shape =
    case shape of
        M.Point _ ->
            1

        M.Rectangle { rectangle } ->
            Rectangle2d.area rectangle |> Quantity.sqrt |> M.inImagePixels

        M.Polygon { polygon } ->
            Polygon2d.area polygon |> Quantity.sqrt |> M.inImagePixels


getRectangleHandle : M.ImagePixels -> M.ImagePoint -> M.RectangleShape -> Maybe M.AnnotationHandle
getRectangleHandle tolerance point r =
    let
        loop handlers =
            case handlers of
                g :: rest ->
                    case g tolerance point r of
                        Just h ->
                            Just h

                        Nothing ->
                            loop rest

                [] ->
                    Nothing
    in
    loop [ getRectangleHandleCorner, getRectangleHandleMove ]


getRectangleHandleCorner : M.ImagePixels -> M.ImagePoint -> M.RectangleShape -> Maybe M.AnnotationHandle
getRectangleHandleCorner tolerance point r =
    let
        vertices =
            Rectangle2d.vertices r.rectangle |> Array.fromList

        getHdl i =
            case ( Array.get i vertices, Array.get ((i + 2) // 4) vertices ) of
                ( Just p, Just anc ) ->
                    if Point2d.equalWithin tolerance p point then
                        Just (M.RectangleEditCorner { oppositeCorner = anc, control = p })

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


getRectangleHandleMove : M.ImagePixels -> M.ImagePoint -> M.RectangleShape -> Maybe M.AnnotationHandle
getRectangleHandleMove _ point r =
    if Rectangle2d.contains point r.rectangle then
        Just (M.RectangleMove r { start = point, control = point })

    else
        Nothing
