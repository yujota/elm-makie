module Conventional exposing (main)

import Browser
import Html exposing (Html)
import Makie exposing (Event, Makie)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Makie, Cmd Msg )
init _ =
    ( Makie.makie { name = "sample", width = 3456, height = 5184, src = imageSrc }, Cmd.none )


type Msg
    = Msg Event


update : Msg -> Makie -> ( Makie, Cmd Msg )
update (Msg event) m =
    ( Makie.update event m, Cmd.none )


view : Makie -> Html Msg
view m =
    Makie.view Msg m


subscriptions : Makie -> Sub Msg
subscriptions =
    Makie.subscriptions Msg


imageSrc =
    "https://images.pexels.com/photos/245035/pexels-photo-245035.jpeg?cs=srgb&dl=adorable-animal-blur-breed-245035.jpg&fm=jpg"
