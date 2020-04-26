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
import Makie.Internal.Gestures
import Makie.Internal.Makie as M
import Makie.Internal.ObjectContainer
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
        { camera =
            Makie.Internal.Camera.camera
                { imageWidth = r.width
                , imageHeight = r.height
                , paneWidth = 640
                , paneHeight = 480
                }
        , target = M.NoTarget
        , mode = M.BrowseMode
        , imageWidth = r.width
        , imageHeight = r.height
        , paneWidth = 640
        , paneHeight = 480
        , annotations = Makie.Internal.ObjectContainer.objectContainer { depth = 4, unitSize = 256 }
        , gestureModel = Makie.Internal.Gestures.gestureModel
        , defaultLabel = Nothing
        , contents = Makie.Internal.Canvas.singleImageCanvasContents { src = r.src }
        , renderedTime = Time.millisToPosix 0
        }


interpret : Event -> Makie -> ( Makie, Action )
interpret e (M.Makie r) =
    Makie.Internal.Events.interpret e r |> Tuple.mapFirst M.Makie


apply : Action -> Makie -> Makie
apply a ((M.Makie r) as m) =
    -- case Debug.log "action" a of
    case a of
        M.NoAction ->
            m

        M.Batch actions ->
            -- TODO
            m

        M.Move paneVector ->
            Makie.Internal.Camera.move paneVector r.camera |> (\c -> M.Makie { r | camera = c })

        M.Zoom panePoint reductionRate ->
            Makie.Internal.Camera.zoom panePoint reductionRate r.camera |> (\c -> M.Makie { r | camera = c })

        M.Rotate panePoint angle ->
            Makie.Internal.Camera.rotate panePoint angle r.camera |> (\c -> M.Makie { r | camera = c })

        M.Add annotationRecord ->
            m

        M.Insert key annotationRecord ->
            m

        M.Delete key annotationRecord ->
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
