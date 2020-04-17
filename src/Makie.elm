module Makie exposing
    ( Event
    , Makie
    , apply
    , handle
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
import Html exposing (Html)
import Html.Attributes
import Makie.Canvas
import Makie.Events
import Makie.Internal.Camera
import Makie.Internal.Canvas
import Makie.Internal.Events
import Makie.Internal.Makie as M
import Time exposing (Posix)


type alias Makie =
    M.Makie


type alias Event =
    M.Event


type alias Action =
    M.Action


makie : { src : String, width : Int, height : Int, name : String } -> Makie
makie r =
    M.Makie
        { event = M.initialEventStatus
        , imageWidth = r.width
        , imageHeight = r.height
        , paneWidth = 640
        , paneHeight = 480
        , camera =
            Makie.Internal.Camera.camera
                { imageWidth = r.width
                , imageHeight = r.height
                , paneWidth = 640
                , paneHeight = 480
                }
        , contents = Makie.Internal.Canvas.singleImageCanvasContents { src = r.src }
        }


handle : Event -> Makie -> ( Makie, Action )
handle e ((M.Makie r) as m) =
    case e of
        M.PointerEventVariant pointerEvent ->
            Makie.Internal.Events.handlePointerEvent pointerEvent r.event
                |> Tuple.mapFirst (\ev -> M.Makie { r | event = ev })

        M.RefreshPane posix ->
            case r.contents of
                M.SingleImageCanvasContents c ->
                    Makie.Internal.Canvas.renderSingleImageCanvas
                        { paneWidth = r.paneWidth
                        , paneHeight = r.paneHeight
                        , imageWidth = r.imageWidth
                        , imageHeight = r.imageHeight
                        , camera = r.camera
                        }
                        c
                        |> (\cnt -> ( M.Makie { r | contents = M.SingleImageCanvasContents cnt }, M.NoAction ))

        M.SingleImageCanvasTextureLoaded texture ->
            case r.contents of
                M.SingleImageCanvasContents c ->
                    Makie.Internal.Canvas.handleSingleImageCanvasTextureLoaded texture c
                        |> (\cnt ->
                                ( M.Makie { r | contents = M.SingleImageCanvasContents cnt } |> M.requestRendering
                                , M.NoAction
                                )
                           )


apply : Action -> Makie -> Makie
apply a ((M.Makie r) as m) =
    case Debug.log "action" a of
        M.CameraActionVariant cameraAction ->
            Makie.Internal.Camera.apply cameraAction r.camera |> (\c -> M.Makie { r | camera = c })

        M.AnnotationActionVariant annotationAction ->
            -- TODO
            m

        M.NoAction ->
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



-- Conventional


update : Event -> Makie -> Makie
update e m =
    handle e m |> (\( mak, act ) -> apply act mak)


view : (Event -> msg) -> Makie -> Html msg
view toMessage ((M.Makie r) as m) =
    Canvas.toHtmlWith
        { width = paneWidth m, height = paneHeight m, textures = Makie.Canvas.textures m toMessage }
        (List.map (Html.Attributes.map toMessage) Makie.Events.onPointerOperations)
        (Makie.Canvas.renderables m)


subscriptions : (Event -> msg) -> Makie -> Sub msg
subscriptions toMessage _ =
    onAnimationFrame (refreshPane >> toMessage)
