module Makie exposing
    ( Event
    , Makie
    , apply
    , browseMode
    , interpret
    , makie
    , paneHeight
    , paneWidth
    , pointAnnotationMode
    , rectangleAnnotationMode
    , refreshPane
    , subscriptions
    , update
    , view
    )

import BoundingBox2d
import Browser.Events exposing (onAnimationFrame)
import Canvas
import CollisionDetection2d
import Color
import Html exposing (Html)
import Html.Attributes
import Makie.Canvas
import Makie.Events
import Makie.Internal.Annotations
import Makie.Internal.Camera
import Makie.Internal.Canvas
import Makie.Internal.Events
import Makie.Internal.Gestures
import Makie.Internal.Labels
import Makie.Internal.Makie as M
import Prng.Uuid
import Random.Pcg.Extended
import Time exposing (Posix)


type alias Makie =
    M.Makie


type alias Event =
    M.Event


type alias Action =
    M.Action


type alias IdGenerator =
    M.IdGenerator


makie : { src : String, width : Int, height : Int, name : String } -> Makie
makie r =
    let
        ( iWidth, iHeight ) =
            ( r.width, r.height )

        ( pWidth, pHeight ) =
            ( 640 * 2, 480 * 2 )
    in
    M.Makie
        { camera =
            Makie.Internal.Camera.camera
                { imageWidth = iWidth
                , imageHeight = iHeight
                , paneWidth = pWidth
                , paneHeight = pHeight
                }
        , target = M.NoTarget
        , mode = M.BrowseMode
        , imageWidth = r.width
        , imageHeight = r.height
        , paneWidth = pWidth
        , paneHeight = pHeight
        , annotations = emptyAnnotationContainer (toFloat iWidth) (toFloat iHeight)
        , gestureModel = Makie.Internal.Gestures.gestureModel

        -- , defaultLabel = Nothing
        , defaultLabel =
            M.CustomLabel
                { name = "sample"
                , colors = { lineColor = Color.orange, selectedLineColor = Color.red, fillColor = Color.orange, selectedFillColor = Color.red }
                }
                |> Just
        , categories = Makie.Internal.Labels.emptyCategories
        , display = Makie.Internal.Canvas.initDisplay { src = r.src, paneWidth = pWidth, paneHeight = pHeight }
        , renderedTime = Time.millisToPosix 0
        , idGenerator = incrementalIdGenerator { initialCategoryId = 0, initialAnnotationId = 0 }
        }


incrementalIdGenerator : { initialCategoryId : Int, initialAnnotationId : Int } -> IdGenerator
incrementalIdGenerator { initialCategoryId, initialAnnotationId } =
    M.Incremental { category = initialCategoryId, annotation = initialAnnotationId }


uuidGenerator : { seed : Int, seedExtension : List Int } -> IdGenerator
uuidGenerator { seed, seedExtension } =
    M.UseUuid (Random.Pcg.Extended.initialSeed seed seedExtension)


interpret : Event -> Makie -> ( Makie, Action )
interpret e (M.Makie r) =
    Makie.Internal.Events.interpret e r |> Tuple.mapFirst M.Makie


apply : Action -> Makie -> Makie
apply a (M.Makie m) =
    -- case Debug.log "action" a of
    case a of
        M.NoAction ->
            M.Makie m

        M.Batch actions ->
            List.foldl (\ac mak -> apply ac mak) (M.Makie m) actions

        M.Move paneVector ->
            Makie.Internal.Camera.move paneVector m.camera
                |> (\c -> { m | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Zoom panePoint reductionRate ->
            Makie.Internal.Camera.zoom panePoint reductionRate m.camera
                |> (\c -> { m | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Rotate panePoint angle ->
            Makie.Internal.Camera.rotate panePoint angle m.camera
                |> (\c -> { m | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Add annotationRecord ->
            let
                ( key, mki ) =
                    generateNewId True m
            in
            apply (M.Insert key annotationRecord) (M.Makie mki)

        M.Insert key annotationRecord ->
            { m | annotations = CollisionDetection2d.insert key annotationRecord m.annotations }
                |> (\mki ->
                        if Makie.Internal.Annotations.isVisible mki annotationRecord then
                            -- TODO 古いアノテーションが存在していれば.. それらを消す必要がある.
                            mki |> Makie.Internal.Canvas.requestRenderAnnotations |> M.Makie

                        else
                            mki |> M.Makie
                   )

        M.Delete key ->
            (\mki -> M.Makie { mki | annotations = CollisionDetection2d.remove key mki.annotations }) <|
                case Maybe.map (Makie.Internal.Annotations.isVisible m) (CollisionDetection2d.get key m.annotations) of
                    Just True ->
                        m |> Makie.Internal.Canvas.requestRenderAnnotations

                    _ ->
                        m


refreshPane : Posix -> Event
refreshPane =
    M.RefreshPane


paneWidth : Makie -> Int
paneWidth (M.Makie r) =
    r.paneWidth


paneHeight : Makie -> Int
paneHeight (M.Makie r) =
    r.paneHeight


browseMode : Makie -> Makie
browseMode =
    setMode M.BrowseMode


pointAnnotationMode : Makie -> Makie
pointAnnotationMode =
    setMode M.PointMode


rectangleAnnotationMode : Makie -> Makie
rectangleAnnotationMode =
    setMode M.RectangleMode



-- Conventional


update : Event -> Makie -> Makie
update e m =
    interpret e m |> (\( mak, act ) -> apply act mak)


view : (Event -> msg) -> Makie -> Html msg
view toMessage ((M.Makie r) as m) =
    Canvas.toHtmlWith
        { width = paneWidth m, height = paneHeight m, textures = Makie.Canvas.textures m toMessage }
        (List.map (Html.Attributes.map toMessage) (Makie.Events.onPointerEvents ++ Makie.Events.onWheelEvents))
        (Makie.Canvas.renderables m)


subscriptions : (Event -> msg) -> Makie -> Sub msg
subscriptions toMessage _ =
    onAnimationFrame (refreshPane >> toMessage)



-- Helpers


generateNewId : Bool -> M.MakieRecord -> ( String, M.MakieRecord )
generateNewId isAnnotationId ({ idGenerator } as m) =
    case ( idGenerator, isAnnotationId ) of
        ( M.Incremental r, True ) ->
            ( String.fromInt r.annotation, { m | idGenerator = M.Incremental { r | annotation = r.annotation + 1 } } )

        ( M.Incremental r, False ) ->
            ( String.fromInt r.category, { m | idGenerator = M.Incremental { r | category = r.category + 1 } } )

        ( M.UseUuid seed, _ ) ->
            let
                ( newUuid, newSeed ) =
                    Random.Pcg.Extended.step Prng.Uuid.generator seed
            in
            ( Prng.Uuid.toString newUuid, { m | idGenerator = M.UseUuid newSeed } )


setMode : M.Mode -> M.Makie -> M.Makie
setMode mode (M.Makie r) =
    M.Makie { r | mode = mode }


emptyAnnotationContainer : Float -> Float -> CollisionDetection2d.Container String M.AnnotationRecord M.ImageBoundingBox
emptyAnnotationContainer imageWidth imageHeight =
    CollisionDetection2d.quadTree
        { extrema =
            BoundingBox2d.extrema
                >> (\r ->
                        { minX = M.inImagePixels r.minX
                        , minY = M.inImagePixels r.minY
                        , maxX = M.inImagePixels r.maxX
                        , maxY = M.inImagePixels r.maxY
                        }
                   )
        , intersects = BoundingBox2d.intersects
        , getBoundingBox = Makie.Internal.Annotations.boundingBox
        , boundary = { minX = 0, minY = 0, maxX = imageWidth, maxY = imageHeight }
        }


emptyAnnotationContainer2 : Float -> Float -> CollisionDetection2d.Container String M.AnnotationRecord M.ImageBoundingBox
emptyAnnotationContainer2 imageWidth imageHeight =
    CollisionDetection2d.naive
        { extrema =
            BoundingBox2d.extrema
                >> (\r ->
                        { minX = M.inImagePixels r.minX
                        , minY = M.inImagePixels r.minY
                        , maxX = M.inImagePixels r.maxX
                        , maxY = M.inImagePixels r.maxY
                        }
                   )
        , intersects = BoundingBox2d.intersects
        , getBoundingBox = Makie.Internal.Annotations.boundingBox
        }
