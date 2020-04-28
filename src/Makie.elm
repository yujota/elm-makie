module Makie exposing
    ( Event
    , Makie
    , apply
    , interpret
    , makie
    , paneHeight
    , paneWidth
    , refreshPane
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onAnimationFrame)
import Canvas
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
import Makie.Internal.ObjectContainer
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
    M.Makie
        { camera =
            Makie.Internal.Camera.camera
                { imageWidth = r.width
                , imageHeight = r.height
                , paneWidth = 640
                , paneHeight = 480
                }
        , target = M.NoTarget

        -- , mode = M.BrowseMode
        , mode = M.PointMode
        , imageWidth = r.width
        , imageHeight = r.height
        , paneWidth = 640
        , paneHeight = 480
        , annotations = Makie.Internal.ObjectContainer.objectContainer { depth = 4, unitSize = 256 }
        , gestureModel = Makie.Internal.Gestures.gestureModel

        -- , defaultLabel = Nothing
        , defaultLabel =
            M.CustomLabel
                { name = "sample"
                , colors = { lineColor = Color.orange, selectedLineColor = Color.red, fillColor = Color.orange, selectedFillColor = Color.red }
                }
                |> Just
        , categories = Makie.Internal.Labels.emptyCategories
        , display = Makie.Internal.Canvas.initDisplay { src = r.src }
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
apply a ((M.Makie r) as m) =
    case Debug.log "action" a of
        -- case a of
        M.NoAction ->
            m

        M.Batch actions ->
            List.foldl (\ac mak -> apply ac mak) m actions

        M.Move paneVector ->
            Makie.Internal.Camera.move paneVector r.camera
                |> (\c -> { r | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Zoom panePoint reductionRate ->
            Makie.Internal.Camera.zoom panePoint reductionRate r.camera
                |> (\c -> { r | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Rotate panePoint angle ->
            Makie.Internal.Camera.rotate panePoint angle r.camera
                |> (\c -> { r | camera = c })
                |> Makie.Internal.Canvas.requestRendering
                |> M.Makie

        M.Add annotationRecord ->
            let
                ( key, mki ) =
                    generateNewId True r
            in
            apply (M.Insert key annotationRecord) (M.Makie mki)

        M.Insert key annotationRecord ->
            Makie.Internal.Annotations.insert key annotationRecord r
                |> (\mki ->
                        if Makie.Internal.Annotations.isVisible mki annotationRecord then
                            mki |> Makie.Internal.Canvas.requestRenderAnnotations |> M.Makie

                        else
                            mki |> M.Makie
                   )

        M.Delete key ->
            case Makie.Internal.Annotations.pop key r of
                ( Just ant, mki ) ->
                    if Makie.Internal.Annotations.isVisible mki ant then
                        mki |> Makie.Internal.Canvas.requestRenderAnnotations |> M.Makie

                    else
                        mki |> M.Makie

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
