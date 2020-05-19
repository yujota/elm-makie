module Simple exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Makie exposing (Event, Makie)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Makie, Cmd Msg )
init _ =
    ( Makie.makie { name = "sample", width = 3456, height = 5184, src = imageSrc }, Cmd.none )


type Msg
    = EventTriggered Event
    | NewMode (Makie -> Makie)


update : Msg -> Makie -> ( Makie, Cmd Msg )
update msg m =
    case msg of
        EventTriggered event ->
            ( Makie.update event m, Cmd.none )

        NewMode f ->
            ( f m, Cmd.none )


view : Makie -> Html Msg
view m =
    Html.div [ Html.Attributes.style "padding-left" "30px" ]
        [ Html.h1 [] [ Html.text "Title" ]
        , Html.div []
            [ Html.div []
                [ Html.button [ onClick (NewMode Makie.browseMode) ] [ Html.text "Browse Mode" ]
                , Html.button [ onClick (NewMode Makie.pointAnnotationMode) ] [ Html.text "Point Mode" ]
                , Html.button [ onClick (NewMode Makie.rectangleAnnotationMode) ] [ Html.text "Rectangle Mode" ]
                ]
            , Html.div []
                [ Makie.view EventTriggered m
                ]
            ]
        ]


subscriptions : Makie -> Sub Msg
subscriptions =
    Makie.subscriptions EventTriggered


imageSrc =
    "https://images.pexels.com/photos/245035/pexels-photo-245035.jpeg?cs=srgb&dl=adorable-animal-blur-breed-245035.jpg&fm=jpg"
