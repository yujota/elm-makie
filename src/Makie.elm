module Makie exposing
    ( Event
    , Image
    , Makie
    , apply
    , handle
    , init
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
import Makie.Actions as Actions exposing (Action)
import Makie.Canvas
import Makie.Events exposing (PointerEventSpec)
import Makie.Internal.Makie as M exposing (Image, Makie)
import Time exposing (Posix)


type alias Makie =
    Makie


type alias Event =
    Event


type alias Action =
    Action


type alias Image =
    Image


makie : { image : Image } -> Makie
makie r =
    M.Makie { eventStatus = M.initialEventStatus, image = r.image, paneWidth = 640, paneHeight = 480 }


image : { src : String, name : String, width : Int, height : Int } -> Image
image r =
    M.Image r


handle : Event -> Makie -> ( Makie, Action )
handle e m =
    -- TODO
    ( m, Actions.none )


apply : Action -> Makie -> Makie
apply a m =
    -- TODO
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


init : { src : String, width : Int, height : Int, name : String } -> Makie
init r =
    image r |> (\i -> makie { image = i })


update : Event -> Makie -> Makie
update e m =
    handle e m |> (\( mak, act ) -> apply act mak)


view : (Event -> msg) -> Makie -> Html msg
view toMessage ((M.Makie r) as m) =
    Canvas.toHtmlWith
        { width = paneWidth m, height = paneHeight m, textures = Makie.Canvas.textures m toMessage }
        Makie.Events.onPointerOperations
        (Makie.Canvas.renderables m)


subscriptions : (Event -> msg) -> Makie -> Sub msg
subscriptions toMessage _ =
    onAnimationFrame (refreshPane >> toMessage)
