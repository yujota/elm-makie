module Makie.Internal.AnnotationContainer exposing
    ( getIndicesTouched
    , getLinerQuaternaryTreeIndex
    , getQuadKey
    , insert
    , objectContainer
    , remove
    , touched
    )

import Array
import Binary
import BoundingBox2d
import Dict
import Makie.Internal.Makie as M
import Point2d
import Set exposing (Set)


objectContainer : { depth : Int, unitSize : Int } -> M.ObjectContainer o
objectContainer { depth, unitSize } =
    let
        numIndices =
            (4 ^ depth - 1) // 3
    in
    { linerQuaternaryTree = Array.repeat numIndices Set.empty
    , objects = Dict.empty
    , depth = depth
    , unitSize = unitSize
    }


insert : String -> M.ImageBoundingBox -> o -> M.ObjectContainer o -> M.ObjectContainer o
insert key boundingBox object container =
    let
        newIndex =
            getLinerQuaternaryTreeIndex { depth = container.depth, unitSize = container.unitSize } boundingBox

        updateLQTIndex c =
            insertLqtIndex newIndex key <|
                case Dict.get key container.objects of
                    Just oldData ->
                        removeLqtIndex oldData.index key c

                    Nothing ->
                        c
    in
    updateLQTIndex container
        |> (\c ->
                { c
                    | objects =
                        Dict.insert key
                            { index = newIndex, object = object, boundingBox = boundingBox }
                            c.objects
                }
           )


remove : String -> M.ObjectContainer o -> M.ObjectContainer o
remove key container =
    case Dict.get key container.objects of
        Just data ->
            { container | objects = Dict.remove key container.objects }
                |> removeLqtIndex data.index key

        Nothing ->
            container


touched : (o -> Bool) -> M.ImageBoundingBox -> M.ObjectContainer o -> List o
touched checkInDetail boundingBox container =
    let
        filter a =
            case checkInDetail a of
                True ->
                    Just a

                False ->
                    Nothing
    in
    getIndicesTouched boundingBox container
        |> List.concatMap
            (\i ->
                Array.get i container.linerQuaternaryTree
                    |> Maybe.map Set.toList
                    |> Maybe.withDefault []
                    |> List.filterMap (\n -> Dict.get n container.objects |> Maybe.andThen (.object >> filter))
            )


{-| Calculate liner quaternary tree index. `depth` must be equal or larger than 2.

    bBox = BoundingBox2d.from (imagePoint { x = 10, y = 10 }) (imagePoint { x = 15, y = 15 })
    getLinerQuaternaryTreeIndex { depth = 2, unitSize = 20 } bBox == 1  -- Number 0 element of Level 1.

-}
getLinerQuaternaryTreeIndex : { depth : Int, unitSize : Int } -> M.ImageBoundingBox -> Int
getLinerQuaternaryTreeIndex { depth, unitSize } boundingBox =
    let
        ( topLeft, bottomRight ) =
            BoundingBox2d.extrema boundingBox
                |> (\r ->
                        ( Point2d.xy r.minX r.minY |> getQuadKey unitSize
                        , Point2d.xy r.maxX r.maxY |> getQuadKey unitSize
                        )
                   )

        padZeros l =
            List.repeat (max 0 (2 * (depth - 1) - List.length l)) 0 ++ l

        totalTilesOfUpperLayers : Int -> Int
        totalTilesOfUpperLayers currentLevel =
            -- Root Level is 0; Start counting with 0
            (4 ^ currentLevel - 1) // 3

        getIndex : List ( Int, ( Int, Int ) ) -> Int
        getIndex l =
            case l of
                ( i, ( a, b ) ) :: [] ->
                    if a == 0 && b == 0 then
                        -- When bounding box is contained in most-leaf level tile. ( i + 1 == depth ).
                        topLeft |> Binary.toDecimal |> (+) (totalTilesOfUpperLayers (i + 1))

                    else
                        -- When bounding box is contained in (depth - 1) level tile. ( Level == depth - 1 == i ).
                        Binary.shiftRightZfBy (2 * (depth - i - 1)) topLeft
                            |> Binary.toDecimal
                            |> (+) (totalTilesOfUpperLayers i)

                ( i, ( a, b ) ) :: rest ->
                    if a == 0 && b == 0 then
                        getIndex rest

                    else
                        -- When bounding box is contained in i level tile.
                        Binary.shiftRightZfBy (2 * (depth - i - 1)) topLeft
                            |> Binary.toDecimal
                            |> (+) (totalTilesOfUpperLayers i)

                [] ->
                    0
    in
    Binary.xor topLeft bottomRight
        |> Binary.toIntegers
        |> padZeros
        |> groupsOfTwo
        |> List.indexedMap (\i t -> ( i, t ))
        |> getIndex


getQuadKey : Int -> M.ImagePoint -> Binary.Bits
getQuadKey unitSize point =
    -- TODO
    Point2d.toRecord M.inImagePixels point
        |> (\r ->
                { row =
                    floor r.x
                        // unitSize
                        |> Binary.fromDecimal
                        |> Binary.toIntegers
                        |> List.concatMap (\i -> [ 0, i ])
                        |> Binary.fromIntegers
                , col =
                    floor r.y
                        // unitSize
                        |> Binary.fromDecimal
                        |> Binary.toIntegers
                        |> List.concatMap (\i -> [ i, 0 ])
                        |> Binary.fromIntegers
                }
           )
        |> (\r -> Binary.or r.col r.row)


getIndicesTouched : M.ImageBoundingBox -> M.ObjectContainer o -> List Int
getIndicesTouched boundingBox { unitSize, depth } =
    let
        div i =
            (M.inImagePixels i |> floor) // unitSize
    in
    BoundingBox2d.extrema boundingBox
        |> (\r -> combinedRange { startX = div r.minX, endX = div r.maxX, startY = div r.minY, endY = div r.maxY })
        |> List.map xy2quadKey
        |> getIndicesTouchedLoop (depth - 1) []



-- Helper functions


insertLqtIndex : Int -> String -> M.ObjectContainer o -> M.ObjectContainer o
insertLqtIndex index key container =
    case Array.get index container.linerQuaternaryTree of
        Just s ->
            { container | linerQuaternaryTree = Array.set index (Set.insert key s) container.linerQuaternaryTree }

        Nothing ->
            container


removeLqtIndex : Int -> String -> M.ObjectContainer o -> M.ObjectContainer o
removeLqtIndex index key container =
    case Array.get index container.linerQuaternaryTree of
        Just s ->
            { container | linerQuaternaryTree = Array.set index (Set.remove key s) container.linerQuaternaryTree }

        Nothing ->
            container


getIndicesTouchedLoop : Int -> List Int -> List Binary.Bits -> List Int
getIndicesTouchedLoop level indices bits =
    let
        addition =
            (4 ^ level - 1) // 3

        toSearch =
            bits
                |> List.map (Binary.shiftRightZfBy 2 >> Binary.toDecimal)
                -- Remove duplicates
                |> Set.fromList
                |> Set.toList
                |> List.map Binary.fromDecimal

        newIndices =
            List.map (Binary.toDecimal >> (+) addition) bits ++ indices
    in
    if level < 0 then
        indices

    else
        getIndicesTouchedLoop (level - 1) newIndices toSearch


xy2quadKey : ( Int, Int ) -> Binary.Bits
xy2quadKey ( x, y ) =
    ( Binary.fromDecimal x |> Binary.toIntegers |> List.concatMap (\i -> [ 0, i ]) |> Binary.fromIntegers
    , Binary.fromDecimal y |> Binary.toIntegers |> List.concatMap (\i -> [ i, 0 ]) |> Binary.fromIntegers
    )
        |> (\( a, b ) -> Binary.or a b)


combinedRange : { startX : Int, endX : Int, startY : Int, endY : Int } -> List ( Int, Int )
combinedRange { startX, endX, startY, endY } =
    let
        rows y =
            List.range startX endX |> List.map (\x -> ( x, y ))
    in
    List.range startY endY |> List.map rows |> List.concat


groupsOfTwo : List Int -> List ( Int, Int )
groupsOfTwo l =
    case l of
        a :: b :: rest ->
            ( a, b ) :: groupsOfTwo rest

        _ ->
            []
