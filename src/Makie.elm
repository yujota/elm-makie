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
        { eventStatus = M.initialEventStatus
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


interpret : Event -> Makie -> ( Makie, Action )
interpret e (M.Makie r) =
    Makie.Internal.Events.interpret e r |> Tuple.mapFirst M.Makie


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
