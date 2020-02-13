module Makie.Internal.Math exposing
    ( DetectTileMode(..)
    , gridIndicesOfTriangle
    , tileIndicesOfExternalTriangles
    , tileIndicesOfRectangle
    , visibleTiles
    )

import Makie.Types.Geometry as G
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate(..))
import Set exposing (Set)


type alias Boundary =
    { maxRowId : Int, maxColumnId : Int }


visibleTiles :
    Bool
    ->
        { frame : G.CFrame
        , canvasWidth : G.CanvasPixels
        , canvasHeight : G.CanvasPixels
        , reductionRate : G.ReductionRate
        }
    ->
        { tileWidth : Float
        , tileHeight : Float
        , layerReductionRate : G.ReductionRate
        , layerWidth : Float
        , layerHeight : Float
        }
    -> Set ( Int, Int )
visibleTiles isRotated { frame, canvasWidth, canvasHeight, reductionRate } { tileWidth, tileHeight, layerReductionRate, layerWidth, layerHeight } =
    -- TODO: 病理画像のサイズがキャンバスよりだいぶ小さい場合に, range の出す要素が巨大すぎて, 重くなるバグを治す.
    let
        rect =
            let
                zero =
                    Pixels.pixels 0

                f ( a, b ) =
                    Point2d.xy a b
                        |> Point2d.placeIn frame
                        |> Point2d.at reductionRate
            in
            { pointA = f ( zero, zero )
            , pointB = f ( zero, canvasHeight )
            , pointC = f ( canvasWidth, zero )
            , pointD = f ( canvasWidth, canvasHeight )
            }

        ( scaledTileWidth, scaledTileHeight ) =
            ( Quantity.at layerReductionRate (Pixels.pixels tileWidth)
            , Quantity.at layerReductionRate (Pixels.pixels tileHeight)
            )

        scaledTileSize =
            { tileWidth = scaledTileWidth, tileHeight = scaledTileHeight }

        boundary =
            let
                maxRow =
                    layerWidth / tileWidth |> floorAndIncrementWhenNotOnBoundary |> (\i -> i - 1) |> max 0

                maxColumn =
                    layerHeight / tileHeight |> floorAndIncrementWhenNotOnBoundary |> (\i -> i - 1) |> max 0
            in
            { maxRowId = maxRow, maxColumnId = maxColumn }

        rectIndices =
            tileIndicesOfRectangle Contact boundary scaledTileSize rect
    in
    if not isRotated || Set.isEmpty rectIndices then
        rectIndices

    else
        tileIndicesOfExternalTriangles G.inPathologyPixels boundary scaledTileSize rect
            |> Set.diff rectIndices
            |> padSurroundingTileIndices


padSurroundingTileIndices : Set ( Int, Int ) -> Set ( Int, Int )
padSurroundingTileIndices indices =
    let
        pad ( x, y ) =
            Set.fromList [ ( x - 1, y - 1 ), ( x - 1, y ), ( x, y - 1 ), ( x, y ) ]
    in
    indices
        |> Set.foldl (\i s -> Set.union s (pad i)) Set.empty


type DetectTileMode
    = Inside
    | Contact


{-| 境界値を含まない
-}
tileIndicesOfRectangle :
    DetectTileMode
    -> Boundary
    -> { tileWidth : Quantity Float units, tileHeight : Quantity Float units }
    ->
        { pointA : Point2d units coordinates
        , pointB : Point2d units coordinates
        , pointC : Point2d units coordinates
        , pointD : Point2d units coordinates
        }
    -> Set ( Int, Int )
tileIndicesOfRectangle mode boundary { tileWidth, tileHeight } { pointA, pointB, pointC, pointD } =
    let
        mapAndGet mapper filter =
            [ pointB, pointC, pointD ]
                |> List.map mapper
                |> List.foldr (\p q -> filter p q) (mapper pointA)

        modifyValue =
            case mode of
                Inside ->
                    -- MEMO: 内部の格子点を求める場合は, xまたはyの最小点より大きいtileのインデックスからスタートする
                    (+) 1

                Contact ->
                    identity

        xStart =
            mapAndGet Point2d.xCoordinate Quantity.min
                |> (\v -> Quantity.ratio v tileWidth)
                |> floor
                |> modifyValue
                |> max 0

        xEnd =
            mapAndGet Point2d.xCoordinate Quantity.max
                |> (\v -> Quantity.ratio v tileWidth)
                |> floor
                |> min boundary.maxRowId

        yStart =
            mapAndGet Point2d.yCoordinate Quantity.min
                |> (\v -> Quantity.ratio v tileWidth)
                |> floor
                |> modifyValue
                |> max 0

        yEnd =
            mapAndGet Point2d.yCoordinate Quantity.max
                |> (\v -> Quantity.ratio v tileWidth)
                |> floor
                |> min boundary.maxColumnId
    in
    List.range xStart xEnd
        |> List.concatMap (\x -> List.range yStart yEnd |> List.map (Tuple.pair x))
        |> Set.fromList


{-| 境界値を含む TODO: 含まないようにすべき
-}
gridIndicesOfTriangle :
    (Quantity Float units -> Float)
    -> Boundary
    -> { tileWidth : Quantity Float units, tileHeight : Quantity Float units }
    ->
        { point : Point2d units coordinates
        , dx : Quantity Float units
        , dy : Quantity Float units
        }
    -> Set ( Int, Int )
gridIndicesOfTriangle qToFloat boundary { tileWidth, tileHeight } { point, dx, dy } =
    let
        ( cX, cY ) =
            ( Point2d.xCoordinate point, Point2d.yCoordinate point )

        xStart =
            Quantity.min cX (Quantity.plus cX dx)
                |> (\p -> Quantity.ratio p tileWidth)
                |> floor
                |> (+) 1
                |> max 0

        xEnd =
            Quantity.max cX (Quantity.plus cX dx)
                |> (\p -> Quantity.ratio p tileWidth)
                |> floorAndDecrementWhenOnBoundary
                |> min boundary.maxRowId

        yConstant =
            Quantity.ratio cY tileHeight

        yLine x =
            let
                { grad, cxF, dyF, cyF, tileHeightF, tileWidthF } =
                    { grad = Quantity.ratio dy (Quantity.negate dx)
                    , cxF = qToFloat cX
                    , cyF = qToFloat cY
                    , dyF = qToFloat dy
                    , tileWidthF = qToFloat tileWidth
                    , tileHeightF = qToFloat tileHeight
                    }
            in
            -- Equation: grad * (x - cX) = y - (cY + dY) when X * tileWidth = x and Y * tileHeight = y
            (grad * ((toFloat x * tileWidthF) - cxF) + cyF + dyF)
                |> (\i -> i / tileHeightF)

        -- |> floorAndIncrementWhenNotOnBoundary
        ( yStart, yEnd ) =
            if qToFloat dy < 0 then
                ( yLine >> floor >> (+) 1 >> max 0
                , always (floorAndDecrementWhenOnBoundary yConstant) >> min boundary.maxColumnId
                )

            else
                ( always (floor yConstant + 1) >> max 0
                , yLine >> floorAndDecrementWhenOnBoundary >> min boundary.maxColumnId
                )
    in
    if abs (qToFloat dy) < 0.00001 then
        Set.empty

    else
        List.range xStart xEnd
            |> List.concatMap (\x -> List.range (yStart x) (yEnd x) |> List.map (Tuple.pair x))
            |> Set.fromList


tileIndicesOfExternalTriangles :
    (Quantity Float units -> Float)
    -> Boundary
    -> { tileWidth : Quantity Float units, tileHeight : Quantity Float units }
    ->
        { pointA : Point2d units coordinates
        , pointB : Point2d units coordinates
        , pointC : Point2d units coordinates
        , pointD : Point2d units coordinates
        }
    -> Set ( Int, Int )
tileIndicesOfExternalTriangles qToFloat boundary ({ tileWidth, tileHeight } as tileSize) { pointA, pointB, pointC, pointD } =
    let
        sortWithThenHead : (a -> a -> Order) -> a -> List a -> a
        sortWithThenHead f item items =
            items
                |> List.sortWith f
                |> List.head
                |> (\maybeItem ->
                        case maybeItem of
                            Just i ->
                                case f i item of
                                    GT ->
                                        item

                                    _ ->
                                        i

                            Nothing ->
                                item
                   )

        reverseOrder : Order -> Order
        reverseOrder od =
            case od of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT

        xAscending p1 p2 =
            Quantity.compare (Point2d.xCoordinate p1) (Point2d.xCoordinate p2)

        xDescending p1 p2 =
            xAscending p1 p2 |> reverseOrder

        yAscending p1 p2 =
            Quantity.compare (Point2d.yCoordinate p1) (Point2d.yCoordinate p2)

        yDescending p1 p2 =
            yAscending p1 p2 |> reverseOrder

        getTriangle ( xSort, ySort ) =
            let
                x =
                    sortWithThenHead xSort pointA [ pointB, pointC, pointD ] |> Point2d.xCoordinate

                y =
                    sortWithThenHead ySort pointA [ pointB, pointC, pointD ] |> Point2d.yCoordinate

                maybeDx =
                    [ pointA, pointB, pointC, pointD ]
                        |> List.filter (\p -> Point2d.yCoordinate p |> Quantity.equalWithin Quantity.zero y)
                        |> List.map (Point2d.xCoordinate >> Quantity.minus x)
                        |> List.sortWith (\x1 x2 -> Quantity.compare (Quantity.abs x1) (Quantity.abs x2))
                        |> List.head

                maybeDy =
                    [ pointA, pointB, pointC, pointD ]
                        |> List.filter (\p -> Point2d.xCoordinate p |> Quantity.equalWithin Quantity.zero x)
                        |> List.map (Point2d.yCoordinate >> Quantity.minus y)
                        |> List.sortWith (\y1 y2 -> Quantity.compare (Quantity.abs y1) (Quantity.abs y2))
                        |> List.head
            in
            case ( maybeDx, maybeDy ) of
                ( Just dx, Just dy ) ->
                    Just { point = Point2d.xy x y, dx = dx, dy = dy }

                _ ->
                    Nothing

        getIndices maybeTriangle =
            case maybeTriangle of
                Just r ->
                    indices r

                Nothing ->
                    Set.empty

        indices ({ point, dx, dy } as r) =
            let
                canBeIgnored v l =
                    v |> Quantity.abs |> Quantity.lessThan l
            in
            case ( canBeIgnored dx tileWidth, canBeIgnored dy tileHeight ) of
                ( False, False ) ->
                    gridIndicesOfTriangle qToFloat boundary tileSize r

                ( _, _ ) ->
                    Set.empty
    in
    [ ( xAscending, yAscending )
    , ( xAscending, yDescending )
    , ( xDescending, yDescending )
    , ( xDescending, yAscending )
    ]
        |> List.map (getTriangle >> getIndices)
        |> List.foldr Set.union Set.empty


floorAndIncrementWhenNotOnBoundary : Float -> Int
floorAndIncrementWhenNotOnBoundary floatValue =
    let
        floored =
            floor floatValue
    in
    if floatValue - toFloat floored < 0.00001 then
        floored

    else
        floored + 1


floorAndDecrementWhenOnBoundary : Float -> Int
floorAndDecrementWhenOnBoundary floatValue =
    let
        floored =
            floor floatValue
    in
    if floatValue - toFloat floored < 0.00001 then
        floored - 1

    else
        floored
