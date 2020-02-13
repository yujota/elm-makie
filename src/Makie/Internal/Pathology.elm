module Makie.Internal.Pathology exposing
    ( Msg
    , Pathology
    , Status
    , constraint
    , getStatus
    , isSameStatus
    , normalImage
    , render
    , tiledImage
    , update
    )

import Angle exposing (Angle)
import Array exposing (Array)
import Canvas as C
import Canvas.Settings.Advanced as CAdvanced
import Canvas.Texture exposing (Texture)
import Dict exposing (Dict)
import Json.Encode as E
import Makie.Internal.Camera as Camera exposing (Camera)
import Makie.Internal.Math
import Makie.Types.Camera
import Makie.Types.Geometry as G
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate(..))
import Set exposing (Set)
import Vector2d


{-| Pathology には, texture dataも保存できるようにする.

texture のsrcは毎回変わるはずなので, local cacheに保存され, メモリのことは考えなくて良い.

-}
type Pathology
    = Pathology PathologyData


type Msg
    = TextureLoaded Int (Maybe Texture) -- TileIndex (Maybe Texture)


type Status
    = Status Int



-- Constructors


tiledImage :
    { url : { x : Int, y : Int, z : Int } -> Maybe String
    , lv0Width : Float
    , lv0Height : Float
    , tileWidth : Float
    , tileHeight : Float
    , layers : List { reductionRate : Float, width : Float, height : Float }
    , name : String
    }
    -> Pathology
tiledImage args =
    let
        layers =
            args.layers
                |> List.indexedMap
                    (\i r ->
                        { reductionRate = G.reductionRate r.reductionRate
                        , width = r.width
                        , height = r.height
                        , layerIndex = i
                        }
                    )
                |> Array.fromList

        tiles : Tiles
        tiles =
            let
                convertToTile t =
                    { id = t.id
                    , url = t.url
                    , link = t.link |> List.filterMap tileIndex
                    , texture = Nothing
                    , lv0Point = t.lv0Point
                    , reductionRate = t.reductionRate
                    }
            in
            collectTiles
                |> List.map convertToTile
                |> Array.fromList

        tileIdToIndicesDict =
            collectTiles
                |> List.indexedMap (\a b -> ( encodeTileId b.id, a ))
                |> Dict.fromList

        tileIndex tIndex =
            tIndex |> encodeTileId |> (\k -> Dict.get k tileIdToIndicesDict)

        collectTiles =
            List.range 0 (Array.length layers - 1)
                |> List.foldl
                    (\i s -> tilesOfLayer (Array.get i layers) (Array.get (i + 1) layers) |> List.append s)
                    []

        tilesOfLayer maybeTargetLayer maybeLowerResolutionLayer =
            case maybeTargetLayer of
                Just targetLayer ->
                    createTilesOfLayer
                        { tileWidth = args.tileWidth
                        , tileHeight = args.tileHeight
                        , targetLayer = targetLayer
                        , lowerResolutionLayer = maybeLowerResolutionLayer
                        , url = args.url
                        }

                Nothing ->
                    []
    in
    Pathology
        { tiles = tiles
        , layers = layers
        , lv0Width = G.pathologyPixels args.lv0Width
        , lv0Height = G.pathologyPixels args.lv0Height
        , tileWidth = args.tileWidth
        , tileHeight = args.tileHeight
        , property = { name = args.name }
        , statusCount = 0
        , tileIndex = tileIndex
        }


normalImage :
    { name : String
    , url : String
    , width : Int
    , height : Int
    }
    -> Pathology
normalImage args =
    let
        w =
            args.width |> toFloat |> G.pathologyPixels

        h =
            args.height |> toFloat |> G.pathologyPixels

        layer =
            { reductionRate = G.reductionRate 1
            , width = toFloat args.width
            , height = toFloat args.height
            , layerIndex = 0
            }

        tile =
            { id = { x = 0, y = 0, z = 0 }
            , url = args.url
            , link = []
            , texture = Nothing
            , reductionRate = G.reductionRate 1
            , lv0Point = G.pPoint 0 0
            }

        tileIndex { x, y, z } =
            if x == 0 && y == 0 && z == 0 then
                Just 0

            else
                Nothing

        data =
            { tiles = Array.initialize 1 (always tile)
            , layers = Array.initialize 1 (always layer)
            , lv0Width = w
            , lv0Height = h
            , tileWidth = toFloat args.width
            , tileHeight = toFloat args.height
            , property = { name = args.name }
            , statusCount = 0
            , tileIndex = tileIndex
            }
    in
    Pathology data



-- Update


update : Msg -> Pathology -> Pathology
update msg (Pathology data) =
    case msg of
        TextureLoaded tileIndex (Just texture) ->
            case Array.get tileIndex data.tiles of
                Just tile ->
                    Array.set tileIndex { tile | texture = Just texture } data.tiles
                        |> (\a -> Pathology { data | tiles = a, statusCount = data.statusCount + 1 })

                Nothing ->
                    Pathology data

        TextureLoaded _ Nothing ->
            Pathology data


getStatus : Pathology -> Status
getStatus (Pathology data) =
    Status data.statusCount


isSameStatus : Status -> Status -> Bool
isSameStatus (Status s1) (Status s2) =
    s1 == s2



-- Render functions


type alias Source =
    { url : String, message : Maybe Texture -> Msg }


{-| この関数はMainのupdateの下で呼ばれることを目的としています.
-}
render :
    Pathology
    -> Camera
    -> { toDraw : List C.Renderable, sources : List Source }
render (Pathology data) camera =
    -- tilesOfBestLayer (Pathology data) camera
    case getBestLayerToVisualize data.layers (Camera.getReductionRate camera) of
        Just layer ->
            let
                toSearch =
                    tilesOfCameraSight (Pathology data) camera layer

                getUrl i =
                    Array.get i data.tiles
                        |> Maybe.andThen
                            (\t ->
                                case t.texture of
                                    -- MEMO: すでにTextureが読み込まれているものはsourceとして表示しない.
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        Just ( i, t.url )
                            )

                getSources ts =
                    ts
                        |> List.filterMap getUrl
                        |> List.map (\( i, u ) -> { url = u, message = TextureLoaded i })

                lowestSources =
                    case lowestResolution data.layers of
                        Just lowestLayer ->
                            if lowestLayer.layerIndex == layer.layerIndex then
                                []

                            else
                                tilesOfCameraSight (Pathology data) camera lowestLayer
                                    |> getSources

                        Nothing ->
                            []
            in
            internalLoopRenderDrawings (Pathology data)
                (renderTile { tileWidth = data.tileWidth, tileHeight = data.tileHeight } camera)
                { depth = 2, toDraw = [], toSearch = toSearch }
                |> (\r -> { toDraw = List.concat r.toDraw, sources = List.append (getSources toSearch) lowestSources })

        Nothing ->
            { toDraw = [], sources = [] }


{-| この関数はMainのupdateの下で呼ばれることを目的としています.

zoom しているときなど, sourcesを必要としないとき用

-}
renderDrawings :
    Pathology
    -> Camera
    -> List C.Renderable
renderDrawings (Pathology data) camera =
    case getBestLayerToVisualize data.layers (Camera.getReductionRate camera) of
        Just layer ->
            internalLoopRenderDrawings (Pathology data)
                (renderTile { tileWidth = data.tileWidth, tileHeight = data.tileHeight } camera)
                { depth = 2, toDraw = [], toSearch = tilesOfCameraSight (Pathology data) camera layer }
                |> (\r -> List.concat r.toDraw)

        Nothing ->
            []


renderTile : { tileWidth : Float, tileHeight : Float } -> Camera -> Tile -> List C.Renderable
renderTile { tileWidth, tileHeight } camera tile =
    let
        scale =
            Quantity.ratio tile.reductionRate (Camera.getReductionRate camera)

        rotation =
            Camera.getAngle camera |> Angle.inRadians

        { x, y } =
            Camera.toCanvasPoint camera tile.lv0Point
                |> Point2d.toPixels
                |> (\r -> { x = r.x, y = r.y })

        { tileCenterX, tileCenterY } =
            tile.lv0Point
                |> Point2d.translateBy
                    (Vector2d.pixels (tileWidth / 2) (tileHeight / 2) |> Vector2d.at tile.reductionRate)
                |> Camera.toCanvasPoint camera
                |> Point2d.toPixels
                |> (\r -> { tileCenterX = r.x, tileCenterY = r.y })
    in
    case tile.texture of
        Just t ->
            [ C.texture
                [ CAdvanced.transform
                    [ CAdvanced.translate x y
                    , CAdvanced.scale scale scale
                    , CAdvanced.rotate (-1 * rotation)
                    ]
                ]
                ( 0, 0 )
                t
            ]

        Nothing ->
            []


{-| Memo: toDrawはより低解像度のものが左に出てくる
-}
internalLoopRenderDrawings :
    Pathology
    -> (Tile -> a)
    -> { depth : Int, toDraw : List a, toSearch : List Int }
    -> { depth : Int, toDraw : List a, toSearch : List Int }
internalLoopRenderDrawings (Pathology data) f r =
    if r.depth <= 0 then
        r

    else
        let
            getTextureOr i =
                case Array.get i data.tiles of
                    Just tile ->
                        case tile.texture of
                            Just _ ->
                                { toDraw = [ f tile ], toSearch = [] }

                            Nothing ->
                                { toDraw = [], toSearch = tile.link }

                    Nothing ->
                        { toDraw = [], toSearch = [] }

            merge r1 r2 =
                { toDraw = List.append r1.toDraw r2.toDraw, toSearch = List.append r1.toSearch r2.toSearch }
        in
        -- MEMO: ここでmergeへの引数がjのほうがさきなのは, jのほうが低解像度(初回操作)または同じ解像度のLayerであるから
        List.foldl (\i j -> merge j (getTextureOr i)) { toDraw = r.toDraw, toSearch = [] } r.toSearch
            |> (\l -> { toDraw = l.toDraw, toSearch = l.toSearch, depth = r.depth - 1 })
            |> internalLoopRenderDrawings (Pathology data) f



-- Functions related to Camera


constraint : Pathology -> Makie.Types.Camera.Constraint
constraint (Pathology data) =
    let
        maxReductionRate =
            data.layers
                |> Array.get (Array.length data.layers - 1)
                |> Maybe.map (\l -> l.reductionRate |> Quantity.multiplyBy 8)
                |> Maybe.withDefault (G.reductionRate 64)
    in
    { lv0Width = data.lv0Width
    , lv0Height = data.lv0Height
    , minReductionRate = G.reductionRate 0.25
    , maxReductionRate = maxReductionRate
    }



-- Accessor to properties


name : Pathology -> String
name (Pathology data) =
    data.property.name



-- ===================================================================================================
-- Private functions


{-| この型は, Pathology Slideの構造情報, レイヤ, 倍率のみ把握する
-}
type alias PathologyData =
    { tiles : Tiles
    , layers : Layers
    , lv0Width : G.PathologyPixels -- Layers [0] の情報とかぶるが, Maybeにならない値として保持する
    , lv0Height : G.PathologyPixels
    , tileWidth : Float
    , tileHeight : Float
    , tileIndex : { x : Int, y : Int, z : Int } -> Maybe Int
    , property : Property
    , statusCount : Int
    }


{-| ArrayのインデックスがTileIdのエンコードされたIntを表す

データ型がArrayなのは, 基本的にデータが不変でインデックスアクセスが早そうだから

-}
type alias Tiles =
    Array Tile


type alias Tile =
    { id : TileId
    , url : String
    , link : TileLink
    , texture : Maybe Texture
    , lv0Point : G.PPoint
    , reductionRate : G.ReductionRate
    }


{-| IntはTileIdのエンコードされたIntを表している

より上(低解像度)のレイヤの重なるタイルのIDを保持する

-}
type alias TileLink =
    List Int


type alias TileId =
    { x : Int, y : Int, z : Int }


type alias Layers =
    Array Layer


type alias Layer =
    { reductionRate : G.ReductionRate
    , width : Float
    , height : Float
    , layerIndex : Int
    }


type alias Property =
    { name : String }



-- Helper functions


createTilesOfLayer :
    { tileWidth : Float
    , tileHeight : Float
    , targetLayer : Layer
    , lowerResolutionLayer : Maybe Layer
    , url : TileId -> Maybe String
    }
    ->
        List
            { id : TileId
            , url : String
            , link : List TileId
            , lv0Point : G.PPoint
            , reductionRate : G.ReductionRate
            }
createTilesOfLayer { tileWidth, tileHeight, targetLayer, lowerResolutionLayer, url } =
    let
        countUp : Float -> Float -> Int
        countUp tileSize length =
            let
                v =
                    length / tileSize

                floored =
                    floor v
            in
            if v - toFloat floored < 1 then
                floored

            else
                floored + 1

        combinedRange :
            { startX : Int, endX : Int, startY : Int, endY : Int }
            -> List ( Int, Int )
        combinedRange { startX, endX, startY, endY } =
            let
                rows y =
                    List.range startX endX |> List.map (\x -> ( x, y ))
            in
            List.range startY endY |> List.map rows |> List.concat

        calcLv0Point x y =
            G.pPoint
                (tileWidth * toFloat x * G.inReductionRate targetLayer.reductionRate)
                (tileHeight * toFloat y * G.inReductionRate targetLayer.reductionRate)

        createTile rIndex cIndex =
            let
                tIndex =
                    { x = rIndex, y = cIndex, z = targetLayer.layerIndex }
            in
            (\l ->
                case url tIndex of
                    Just s ->
                        Just
                            { id = tIndex
                            , url = s
                            , link = l
                            , reductionRate = targetLayer.reductionRate
                            , lv0Point = calcLv0Point tIndex.x tIndex.y
                            }

                    Nothing ->
                        Nothing
            )
            <|
                case lowerResolutionLayer of
                    Just lowResLayer ->
                        let
                            scale =
                                Quantity.ratio targetLayer.reductionRate lowResLayer.reductionRate

                            floorAndDecrementWhenOnBoundary floatValue =
                                let
                                    floored =
                                        floor floatValue
                                in
                                if floatValue - toFloat floored < 0.00001 then
                                    floored - 1

                                else
                                    floored

                            startX =
                                toFloat rIndex * scale |> floor

                            endX =
                                (toFloat rIndex + 1) * scale |> floorAndDecrementWhenOnBoundary

                            startY =
                                toFloat cIndex * scale |> floor

                            endY =
                                (toFloat cIndex + 1) * scale |> floorAndDecrementWhenOnBoundary

                            rows y =
                                List.range startX endX |> List.map (\x -> { x = x, y = y, z = lowResLayer.layerIndex })
                        in
                        List.range startY endY |> List.map rows |> List.concat

                    Nothing ->
                        []
    in
    combinedRange
        { startX = 0
        , endX = countUp tileWidth targetLayer.width - 1
        , startY = 0
        , endY = countUp tileHeight targetLayer.height - 1
        }
        |> List.filterMap (\( a, b ) -> createTile a b)


getBestLayerToVisualize : Layers -> G.ReductionRate -> Maybe Layer
getBestLayerToVisualize layers reductionRate =
    case ( highestResolution layers, lowestResolution layers ) of
        ( Just highestLayer, Just lowestLayer ) ->
            let
                ( useHighest, useLowest ) =
                    -- Example of `Quantity.lessThan` : Quantity.lessThan oneMeter (Length.feet 1) -> True
                    ( Quantity.lessThanOrEqualTo highestLayer.reductionRate reductionRate
                    , Quantity.greaterThanOrEqualTo lowestLayer.reductionRate reductionRate
                    )
            in
            case ( useHighest, useLowest ) of
                ( False, False ) ->
                    layers
                        -- 現在の倍率より2倍低解像度のレイヤよりは, 高いものをフィルタしてくる.
                        |> Array.filter
                            (\l ->
                                Quantity.lessThan
                                    (Quantity.multiplyBy 2 reductionRate)
                                    l.reductionRate
                            )
                        |> (\a -> Array.get (Array.length a - 1) a)

                ( False, True ) ->
                    Just lowestLayer

                ( True, _ ) ->
                    Just highestLayer

        _ ->
            Nothing


highestResolution : Layers -> Maybe Layer
highestResolution layers =
    Array.get 0 layers


lowestResolution : Layers -> Maybe Layer
lowestResolution layers =
    Array.get (Array.length layers - 1) layers


tilesOfCameraSight : Pathology -> Camera -> Layer -> List Int
tilesOfCameraSight (Pathology data) camera layer =
    let
        canvasSize =
            Camera.canvasSize camera
    in
    Makie.Internal.Math.visibleTiles
        True
        { frame = Camera.canvasFrame camera
        , canvasWidth = canvasSize.width
        , canvasHeight = canvasSize.height
        , reductionRate = Camera.getReductionRate camera
        }
        { tileWidth = data.tileWidth
        , tileHeight = data.tileHeight
        , layerReductionRate = layer.reductionRate
        , layerWidth = layer.width
        , layerHeight = layer.height
        }
        |> Set.toList
        |> List.filterMap (\( x, y ) -> data.tileIndex { x = x, y = y, z = layer.layerIndex })


encodeTileId : TileId -> String
encodeTileId tileId =
    E.object
        [ ( "x", E.int tileId.x )
        , ( "y", E.int tileId.y )
        , ( "z", E.int tileId.z )
        ]
        |> E.encode 0
