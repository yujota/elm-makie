module Makie.Internal.Annotation exposing
    ( eventCancel
    , eventDelete
    , eventEnd
    , eventEscape
    , eventMove
    , eventStart
    , point
    , render
    )

import BoundingBox2d exposing (BoundingBox2d)
import Canvas as C
import Dict exposing (Dict)
import Makie.Internal.Annotation.Point as Point
import Makie.Internal.Camera as Camera exposing (Camera)
import Makie.Internal.Makie as M
import Makie.Types.Annotation as A
import Makie.Types.Geometry as G
import Makie.Types.State as S
import Pixels
import Point2d exposing (Point2d)
import Time exposing (Posix)


point : Posix -> A.CategoryId -> G.PPoint -> A.Annotation
point createdTime category p =
    { category = category
    , boundingBox = BoundingBox2d.from p p
    , property = A.Point { point = p }
    , createdTime = createdTime
    }


render : S.Params -> S.State msg -> S.State msg
render params ({ annotation } as state) =
    case params.data.status == annotation.replicaStatus of
        True ->
            draw params state

        False ->
            state
                |> syncReplica params
                |> draw params


eventStart : S.Params -> G.CPoint -> S.State msg -> S.State msg
eventStart { project, options } p ({ annotation, camera, mode } as state) =
    let
        reductionRate =
            Camera.getReductionRate camera

        tolerance : G.CanvasPixels
        tolerance =
            Pixels.pixels options.render.touchTolerance

        pPoint =
            Camera.fromCanvasPoint camera p

        newState w =
            { annotation | working = w } |> (\a -> { state | annotation = a })

        getTouched =
            annotation.replica
                |> Dict.foldl
                    (\k v maybeValue ->
                        case maybeValue of
                            Just _ ->
                                maybeValue

                            Nothing ->
                                if isTouched { tolerance = tolerance, reductionRate = reductionRate } pPoint v then
                                    Just ( k, v )

                                else
                                    Nothing
                    )
                    Nothing
    in
    case ( mode, annotation.working ) of
        ( S.BrowseMode, _ ) ->
            case getTouched of
                Just ( antId, ant ) ->
                    S.Selected { id = antId, entity = ant }
                        |> (\w -> { annotation | working = w })
                        |> (\a -> { state | annotation = a, mode = getAnnotationMode ant |> S.AnnotationMode })

                Nothing ->
                    state

        ( S.AnnotationMode antMode, S.NotSelected ) ->
            case ( getTouched, project.selectedCategory ) of
                ( Just ( antId, ant ), _ ) ->
                    S.Selected { id = antId, entity = ant }
                        |> (\w -> { annotation | working = w })
                        |> (\a -> { state | annotation = a, mode = getAnnotationMode ant |> S.AnnotationMode })

                ( Nothing, Just categoryId ) ->
                    S.NewHandle { handle = startNewHandle antMode categoryId p } |> newState

                ( Nothing, Nothing ) ->
                    state

        ( S.AnnotationMode antMode, S.Selected r ) ->
            case isTouched { tolerance = tolerance, reductionRate = reductionRate } pPoint r.entity of
                True ->
                    case
                        startHandle
                            antMode
                            { currentPoint = p
                            , toCanvasPoint = Camera.toCanvasPoint camera
                            , fromCanvasPoint = Camera.fromCanvasPoint camera
                            , tolerance = tolerance
                            , reductionRate = reductionRate
                            }
                            r.entity
                    of
                        Just h ->
                            S.Handled { handle = h, id = r.id, cachedEntity = r.entity } |> newState

                        Nothing ->
                            S.NotSelected |> newState

                False ->
                    case getTouched of
                        Just ( antId, ant ) ->
                            S.Selected { id = antId, entity = ant }
                                |> (\w -> { annotation | working = w })
                                |> (\a -> { state | annotation = a, mode = getAnnotationMode ant |> S.AnnotationMode })

                        Nothing ->
                            S.NotSelected |> newState

        ( S.AnnotationMode _, _ ) ->
            S.NotSelected |> newState


eventMove : G.CPoint -> S.State msg -> S.State msg
eventMove p ({ annotation } as state) =
    (\w -> { annotation | working = w } |> (\a -> { state | annotation = a })) <|
        case annotation.working of
            S.Handled r ->
                updateHandle p r.handle |> (\h -> S.Handled { r | handle = h })

            S.NewHandle r ->
                updateHandle p r.handle |> (\h -> S.NewHandle { r | handle = h })

            _ ->
                annotation.working


eventEnd : S.State msg -> ( S.State msg, List M.Query )
eventEnd ({ annotation, camera, currentTime } as state) =
    let
        newState w =
            { annotation | working = w } |> (\a -> { state | annotation = a })
    in
    case annotation.working of
        S.Handled r ->
            case finishHandle currentTime (Camera.fromCanvasPoint camera) r.handle of
                Ok ant ->
                    ( S.Selected { id = r.id, entity = ant } |> newState
                    , [ M.Insert (M.AnnotationId r.id) (M.Annotation ant) ]
                    )

                Err _ ->
                    -- TODO: Do something with error message
                    ( S.Selected { id = r.id, entity = r.cachedEntity } |> newState
                    , []
                    )

        S.NewHandle r ->
            case finishHandle currentTime (Camera.fromCanvasPoint camera) r.handle of
                Ok ant ->
                    ( S.NotSelected |> newState
                    , [ M.Add (M.Annotation ant) ]
                    )

                Err _ ->
                    -- TODO: Do something with error message
                    ( S.NotSelected |> newState
                    , []
                    )

        _ ->
            ( state, [] )


eventCancel : S.State msg -> S.State msg
eventCancel ({ annotation } as state) =
    (\w -> { annotation | working = w } |> (\a -> { state | annotation = a })) <|
        case annotation.working of
            S.Handled r ->
                S.Selected { id = r.id, entity = r.cachedEntity }

            S.NewHandle _ ->
                S.NotSelected

            _ ->
                annotation.working


eventDelete : S.State msg -> S.State msg
eventDelete ({ annotation } as state) =
    -- TODO
    state


eventEscape : S.State msg -> S.State msg
eventEscape ({ annotation } as state) =
    -- TODO
    state



-- Private


syncReplica : S.Params -> S.State msg -> S.State msg
syncReplica { data } ({ annotation } as state) =
    -- MEMO: この関数は, 今後replicaのデータ構造をモートンオーダに対応させる
    { state | annotation = { annotation | replica = data.annotations, replicaStatus = data.status } }


draw : S.Params -> S.State msg -> S.State msg
draw params ({ annotation, camera } as state) =
    let
        isVisible : A.Annotation -> Bool
        isVisible ant =
            BoundingBox2d.extrema ant.boundingBox
                |> (\r -> ( Point2d.xy r.minX r.minY, Point2d.xy r.maxX r.maxY ))
                |> (\( p1, p2 ) -> Camera.isVisible camera p1 || Camera.isVisible camera p2)

        toCPoint =
            Camera.toCanvasPoint camera

        newAnnotation =
            (\d -> { annotation | toDraw = d }) <|
                case annotation.working of
                    S.NotSelected ->
                        toDraw annotation.replica

                    S.Selected r ->
                        toDraw (Dict.remove r.id annotation.replica)
                            ++ renderSelected params toCPoint r.entity

                    S.Handled r ->
                        toDraw (Dict.remove r.id annotation.replica)
                            ++ renderHandle params r.handle

                    S.NewHandle r ->
                        toDraw annotation.replica
                            ++ renderHandle params r.handle

        toDraw ants =
            ants
                |> Dict.filter (\_ v -> isVisible v)
                |> Dict.values
                |> List.sortBy (\a -> a.createdTime |> Time.posixToMillis)
                |> List.concatMap (renderAnnotation params toCPoint)
    in
    { state | annotation = newAnnotation }


startHandle :
    S.AnnotationModeType
    ->
        { toCanvasPoint : G.PPoint -> G.CPoint
        , fromCanvasPoint : G.CPoint -> G.PPoint
        , currentPoint : G.CPoint
        , tolerance : G.CanvasPixels
        , reductionRate : G.ReductionRate
        }
    -> A.Annotation
    -> Maybe A.Handle
startHandle mode params ant =
    Maybe.map (\h -> { category = ant.category, property = h }) <|
        case ( mode, ant.property ) of
            ( S.PointAnnotationMode, A.Point pty ) ->
                Point.getHandle params pty |> Maybe.map A.PointHandle

            ( _, _ ) ->
                Nothing


startNewHandle : S.AnnotationModeType -> A.CategoryId -> G.CPoint -> A.Handle
startNewHandle mode category p =
    (\h -> { category = category, property = h }) <|
        case mode of
            S.PointAnnotationMode ->
                Point.start p |> A.PointHandle

            _ ->
                -- TODO: Fix
                Point.start p |> A.PointHandle


updateHandle : G.CPoint -> A.Handle -> A.Handle
updateHandle p ({ property } as handle) =
    (\h -> { handle | property = h }) <|
        case property of
            A.PointHandle hdl ->
                Point.updateHandle p hdl |> A.PointHandle


finishHandle : Posix -> (G.CPoint -> G.PPoint) -> A.Handle -> Result String A.Annotation
finishHandle createdTime fromCPoint { category, property } =
    let
        createAnnotation : ( A.Property, G.PBoundingBox ) -> A.Annotation
        createAnnotation ( pty, bBox ) =
            { category = category, boundingBox = bBox, property = pty, createdTime = createdTime }
    in
    Result.map createAnnotation <|
        case property of
            A.PointHandle hdl ->
                Point.finish fromCPoint hdl |> Result.map (\p -> ( A.Point p, Point.getBoundingBox p ))


isTouched : { tolerance : G.CanvasPixels, reductionRate : G.ReductionRate } -> G.PPoint -> A.Annotation -> Bool
isTouched params p ant =
    case ant.property of
        A.Point pty ->
            Point.isTouched params p pty


getAnnotationMode : A.Annotation -> S.AnnotationModeType
getAnnotationMode { property } =
    case property of
        A.Point _ ->
            S.PointAnnotationMode


renderAnnotation : S.Params -> (G.PPoint -> G.CPoint) -> A.Annotation -> List C.Renderable
renderAnnotation { project, options } toCanvasPoint annotation =
    let
        selectedCategory =
            project.selectedCategory
                |> Maybe.andThen (\k -> Dict.get k project.categories)
    in
    case selectedCategory of
        Just category ->
            case annotation.property of
                A.Point pty ->
                    Point.render
                        { toCanvasPoint = toCanvasPoint, color = category.color, radius = options.render.pointRadius }
                        pty

        Nothing ->
            []


renderSelected : S.Params -> (G.PPoint -> G.CPoint) -> A.Annotation -> List C.Renderable
renderSelected { project, options } toCanvasPoint annotation =
    let
        selectedCategory =
            project.selectedCategory
                |> Maybe.andThen (\k -> Dict.get k project.categories)
    in
    case selectedCategory of
        Just category ->
            case annotation.property of
                A.Point pty ->
                    Point.render
                        { toCanvasPoint = toCanvasPoint
                        , color = category.selectedColor
                        , radius = options.render.selectedPointRadius
                        }
                        pty

        Nothing ->
            []


renderHandle : S.Params -> A.Handle -> List C.Renderable
renderHandle { project, options } handle =
    let
        selectedCategory =
            project.selectedCategory
                |> Maybe.andThen (\k -> Dict.get k project.categories)
    in
    case selectedCategory of
        Just category ->
            case handle.property of
                A.PointHandle hdl ->
                    Point.renderHandle
                        { color = category.selectedColor, radius = options.render.selectedPointRadius }
                        hdl

        Nothing ->
            []
