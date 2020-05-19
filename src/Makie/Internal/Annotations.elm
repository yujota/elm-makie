module Makie.Internal.Annotations exposing
    ( boundingBox
    , getHandle
    , getTouched
    , isTouched
    , isVisible
    , shapeTo
    , startPoint
    , startRectangle
    , updateHandle
    , visibleArea
    , visibleItems
    )

import Array
import BoundingBox2d
import CollisionDetection2d
import Makie.Internal.Makie as M
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity, Squared)
import Rectangle2d exposing (Rectangle2d)
import Vector2d



-- Queries


getTouched :
    M.ImagePixels
    -> M.ImagePoint
    -> CollisionDetection2d.Container String M.AnnotationRecord M.ImageBoundingBox
    -> Maybe ( String, M.AnnotationRecord )
getTouched tolerance imagePoint container =
    CollisionDetection2d.collideWith
        (.shape >> isTouched tolerance imagePoint)
        (BoundingBox2d.withDimensions ( tolerance, tolerance ) imagePoint)
        container
        |> List.sortBy (.object >> .shape >> roughSize)
        |> List.head
        |> Maybe.map (\r -> ( r.key, r.object ))


visibleItems :
    { paneWidth : Float, paneHeight : Float, imageWidth : Float, imageHeight : Float }
    -> M.CameraRecord
    -> CollisionDetection2d.Container String M.AnnotationRecord M.ImageBoundingBox
    -> List ( String, M.AnnotationRecord )
visibleItems { paneWidth, paneHeight, imageWidth, imageHeight } camera container =
    let
        cameraArea =
            Rectangle2d.from Point2d.origin (Point2d.pixels paneWidth paneHeight)
                |> Rectangle2d.relativeTo camera.imageFrame
                |> Rectangle2d.at camera.reductionRate

        searchArea =
            visibleArea
                { paneWidth = paneWidth
                , paneHeight = paneHeight
                , imageWidth = imageWidth
                , imageHeight = imageHeight
                }
                camera
    in
    CollisionDetection2d.collideWith (.shape >> isVisibleLogic cameraArea) (Rectangle2d.boundingBox cameraArea) container
        |> List.map (\r -> ( r.key, r.object ))



{-
   -- OC.touched (.shape >> isVisibleLogic cameraArea) (Rectangle2d.boundingBox cameraArea) container

   -- OC.touched (.shape >> isVisibleLogic searchArea) (Rectangle2d.boundingBox cameraArea) container
   OC.touched (always True) (Rectangle2d.boundingBox cameraArea) container
-}


isVisible : M.MakieRecord -> M.AnnotationRecord -> Bool
isVisible { camera, paneWidth, paneHeight } { shape } =
    let
        cameraArea =
            Rectangle2d.from Point2d.origin (Point2d.pixels (toFloat paneWidth) (toFloat paneHeight))
                |> Rectangle2d.relativeTo camera.imageFrame
                |> Rectangle2d.at camera.reductionRate
    in
    isVisibleLogic cameraArea shape


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


updateHandle : M.ImagePoint -> M.AnnotationHandle -> M.AnnotationHandle
updateHandle imagePoint handle =
    case handle of
        M.PointMove pointShape r ->
            M.PointMove pointShape { r | control = imagePoint }

        M.RectangleMove rectangleShape r ->
            M.RectangleMove rectangleShape { r | control = imagePoint }

        M.RectangleEditCorner r ->
            M.RectangleEditCorner { r | control = imagePoint }


shapeTo : M.AnnotationHandle -> M.Shape
shapeTo handle =
    case handle of
        M.PointMove pointShape { start, control } ->
            M.Point { pointShape | point = Point2d.translateBy (Vector2d.from start control) pointShape.point }

        M.RectangleMove rectangleShape { start, control } ->
            M.Rectangle
                { rectangleShape
                    | rectangle = Rectangle2d.translateBy (Vector2d.from start control) rectangleShape.rectangle
                }

        M.RectangleEditCorner { oppositeCorner, control } ->
            M.Rectangle { rectangle = Rectangle2d.from oppositeCorner control }


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


boundingBox : M.AnnotationRecord -> M.ImageBoundingBox
boundingBox { shape } =
    getBoundingBox shape



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


getBoundingBox : M.Shape -> M.ImageBoundingBox
getBoundingBox shape =
    case shape of
        M.Point pointShape ->
            BoundingBox2d.withDimensions ( M.imagePixels 1, M.imagePixels 1 ) pointShape.point

        M.Rectangle rectangleShape ->
            Rectangle2d.boundingBox rectangleShape.rectangle

        M.Polygon polygonShape ->
            polygonShape.boundingBox



-- Collision detection


detectCollisionRect : Rectangle2d a b -> Rectangle2d a b -> Bool
detectCollisionRect rectA rectB =
    List.map (Tuple.pair (\p -> Rectangle2d.contains p rectA)) (Rectangle2d.vertices rectB)
        ++ List.map (Tuple.pair (\p -> Rectangle2d.contains p rectB)) (Rectangle2d.vertices rectA)
        |> detectCollisionLoop


detectCollisionPolygon : Rectangle2d a b -> Polygon2d a b -> Bool
detectCollisionPolygon rect polygon =
    List.map (Tuple.pair (\p -> Polygon2d.contains p polygon)) (Rectangle2d.vertices rect)
        ++ List.map (Tuple.pair (\p -> Rectangle2d.contains p rect)) (Polygon2d.vertices polygon)
        |> detectCollisionLoop


detectCollisionLoop : List ( Point2d a b -> Bool, Point2d a b ) -> Bool
detectCollisionLoop l =
    case l of
        ( f, p ) :: rest ->
            if f p then
                True

            else
                detectCollisionLoop rest

        [] ->
            False


isVisibleLogic : M.ImageRectangle -> M.Shape -> Bool
isVisibleLogic r s =
    case s of
        M.Point { point } ->
            Rectangle2d.contains point r

        M.Rectangle rectangleShape ->
            detectCollisionRect rectangleShape.rectangle r

        M.Polygon polygonShape ->
            detectCollisionPolygon r polygonShape.polygon


visibleArea :
    { paneWidth : Float, paneHeight : Float, imageWidth : Float, imageHeight : Float }
    -> M.CameraRecord
    -> M.ImageRectangle
visibleArea { paneWidth, paneHeight, imageWidth, imageHeight } camera =
    let
        cameraArea =
            Rectangle2d.from Point2d.origin (Point2d.pixels paneWidth paneHeight)
                |> Rectangle2d.relativeTo camera.imageFrame
                |> Rectangle2d.at camera.reductionRate

        imageArea =
            Rectangle2d.from Point2d.origin (M.imagePoint { x = imageWidth, y = imageHeight })
    in
    if List.all (\p -> Rectangle2d.contains p cameraArea) (Rectangle2d.vertices imageArea) then
        imageArea

    else
        cameraArea
