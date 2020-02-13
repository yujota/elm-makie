# yujota/elm-makie

An Elm library to annotate images for supervised learning.

Using this library, users are able to zoom, rotate images and also annotate images for supervised learning.

This library originally aimed to provide an option to display and annotate pathology images.


## Example application

```elm
module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html)
import Makie exposing (Makie)
import Makie.Data as Data exposing (Data)
import Makie.Image as Image exposing (Image)
import Makie.Options as Options
import Makie.Project as Project exposing (Project)
import Time exposing (Posix)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


data : Data
data =
    let
        initialSeed =
            0

        image =
            Image.image (Image.id "sample") { name = "sample", width = 640, height = 480, url = "someUrl" }

        annotations =
            []
    in
    Data.data initialSeed image annotations


type alias Model =
    Makie Msg


type Msg
    = MakieMsg Makie.Msg
    | OnAnimationFrame Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Makie.makie { options = Options.default, project = Project.empty, data = data, lifter = MakieMsg }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakieMsg makieMsg ->
            Makie.update
                { options = Options.default, project = Project.empty, data = data, lifter = MakieMsg }
                makieMsg
                model
                |> (\( m, _ ) -> ( m, Cmd.none ))

        OnAnimationFrame posix ->
            update (Makie.tick posix |> MakieMsg) model


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame OnAnimationFrame


view : Model -> Html Msg
view model =
    Makie.view MakieMsg model
```
