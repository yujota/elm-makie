module Simple exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Color
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Makie exposing (Makie)
import Makie.Category as Category exposing (Category)
import Makie.Data as Data exposing (Data)
import Makie.Image as Image exposing (Image)
import Makie.Mode as Mode exposing (Mode)
import Makie.Options as Options
import Makie.Project as Project exposing (Project)
import Time exposing (Posix)


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { makie : Makie Msg, data : Data }


type Msg
    = MakieMsg Makie.Msg
    | OnAnimationFrame Posix
    | NewMode Mode


init : () -> ( Model, Cmd Msg )
init _ =
    let
        img =
            Image.image (Image.id "sample")
                { name = "sample"
                , width = 3456
                , height = 5184
                , url = dogImageUrl
                }

        data =
            Data.data 0 img []
    in
    ( { makie = Makie.makie { options = Options.default, project = myProject, data = data, lifter = MakieMsg }
      , data = data
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakieMsg makieMsg ->
            Makie.update
                { options = Options.default, project = myProject, data = model.data, lifter = MakieMsg }
                makieMsg
                model.makie
                |> (\( newMakie, queries ) ->
                        ( { model | makie = newMakie, data = Data.update queries model.data }, Cmd.none )
                   )

        NewMode mode ->
            ( { model | makie = Makie.setMode mode model.makie }, Cmd.none )

        OnAnimationFrame posix ->
            update (Makie.tick posix |> MakieMsg) model


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame OnAnimationFrame


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.style "padding-left" "30px" ]
        [ Html.h1 [] [ Html.text "Title" ]
        , Html.div []
            [ Html.button [ onClick (NewMode Mode.browse) ] [ Html.text "Browse Mode" ]
            , Html.button [ onClick (NewMode Mode.point) ] [ Html.text "Point Mode" ]
            ]
        , Makie.view MakieMsg model.makie
        , Html.p []
            [ Html.text
                """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
                        tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
                        eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum.
                        Curabitur pretium tincidunt lacus. Nulla gravida orci a odio. Nullam varius,
                        turpis et commodo pharetra, est eros bibendum elit, nec luctus magna felis sollicitudin mauris.
                        Integer in mauris eu nibh euismod gravida. Duis ac tellus et risus vulputate vehicula.
                        Donec lobortis risus a elit. Etiam tempor. Ut ullamcorper, ligula eu tempor congue,
                        eros est euismod turpis, id tincidunt sapien risus a quam. Maecenas fermentum consequat mi.
                        Donec fermentum. Pellentesque malesuada nulla a mi. Duis sapien sem, aliquet nec, commodo eget,
                        consequat quis, neque. Aliquam faucibus, elit ut dictum aliquet, felis nisl adipiscing sapien,
                        sed malesuada diam lacus eget erat. Cras mollis scelerisque nunc. Nullam arcu.
                        Aliquam consequat. Curabitur augue lorem, dapibus quis, laoreet et, pretium ac, nisi.
                        Aenean magna nisl, mollis quis, molestie eu, feugiat in, orci.
                        In hac habitasse platea dictumst."""
            ]
        ]


dogImageUrl =
    "https://images.pexels.com/photos/245035/pexels-photo-245035.jpeg?cs=srgb&dl=adorable-animal-blur-breed-245035.jpg&fm=jpg"


myProject : Project
myProject =
    let
        myCategory =
            Category.category "my category" Color.green

        myCategoryId =
            Category.id "my category"
    in
    Project.project "my project" [ ( myCategoryId, myCategory ) ] (Just myCategoryId)
