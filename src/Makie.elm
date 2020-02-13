module Makie exposing
    ( Makie, Msg
    , makie
    , update, view, tick
    , getMode, setMode
    )

{-| This module provides API to draw images on canvas (using elm-canvas)


# Types

@docs Makie, Msg


# Constructor

@docs makie


# Update and view

@docs update, view, tick


# Accessor

@docs getMode, setMode

-}

import Array exposing (Array)
import Canvas as C
import Canvas.Settings as CSettings
import Canvas.Texture as Texture exposing (Texture)
import Color
import Element as UI
import Html exposing (Attribute, Html)
import Makie.Data as Data exposing (Data)
import Makie.Internal.Annotation as Annotation
import Makie.Internal.Camera as Camera exposing (Camera)
import Makie.Internal.Event as Event
import Makie.Internal.Makie as M
import Makie.Internal.Pathology as Pathology
import Makie.Mode exposing (Mode)
import Makie.Options exposing (Options)
import Makie.Project exposing (Project)
import Makie.Types.Data as D
import Makie.Types.State as S
import Pixels
import Time exposing (Posix)


{-| Makie type.
-}
type alias Makie msg =
    M.Makie msg


{-| Makie message.
-}
type Msg
    = OnAnimationFrame Posix
    | PathologyMsg Pathology.Msg
    | EventMsg Event.Msg


{-| Constructor
-}
makie : { options : Options, project : Project, data : Data, lifter : Msg -> msg } -> Makie msg
makie ({ lifter } as params_) =
    let
        params =
            extractParams params_

        pathologyImg =
            case params.data.image of
                D.NormalImage r ->
                    Pathology.normalImage { name = r.name, url = r.url, width = r.width, height = r.height }

                D.PathologyImage r ->
                    Pathology.tiledImage
                        { name = r.name
                        , url = r.url
                        , tileWidth = r.tileWidth
                        , tileHeight = r.tileHeight
                        , lv0Width = r.width |> toFloat
                        , lv0Height = r.height |> toFloat
                        , layers = r.layers
                        }

        imgState =
            { pathology = pathologyImg
            , toDraw = []
            , sources = []
            , pathologyStatus = Pathology.getStatus pathologyImg
            }

        antState =
            { replica = params.data.annotations
            , toDraw = []
            , working = S.NotSelected
            , replicaStatus = "init"
            }

        state : S.State msg
        state =
            { camera =
                Camera.create
                    (Pathology.constraint pathologyImg)
                    { width = Pixels.pixels 640, height = Pixels.pixels 480 }
            , width = 640
            , height = 480
            , currentTime = Time.millisToPosix 0
            , mode = S.BrowseMode
            , cameraActions = []
            , image = imgState
            , annotation =
                antState
            , interaction = { isDragging = False, touch = S.NoTouch }
            , error = { errors = Array.empty }
            }
                |> renderImage params lifter
                |> renderAnnotation params lifter
    in
    M.Makie state


{-| Update function
-}
update :
    { options : Options, project : Project, data : Data, lifter : Msg -> msg }
    -> Msg
    -> Makie msg
    -> ( Makie msg, List Data.Query )
update ({ lifter } as params_) msg (M.Makie state) =
    -- TODO: Check if data is changed
    let
        params =
            extractParams params_
    in
    case msg of
        OnAnimationFrame posix ->
            -- TODO: ここのプロセスでdataとstateの比較をする
            reflect params lifter posix state |> (\s -> ( M.Makie s, [] ))

        PathologyMsg pathologyMsg ->
            let
                newPathologyImg =
                    Pathology.update pathologyMsg state.image.pathology

                newImageState img =
                    { img | pathology = newPathologyImg }
            in
            ( M.Makie { state | image = newImageState state.image }, [] )

        EventMsg eventMsg ->
            Event.manage params eventMsg state |> Tuple.mapFirst M.Makie


{-| View
-}
view : (Msg -> msg) -> Makie msg -> Html msg
view lifter (M.Makie ({ width, height, image, annotation } as state)) =
    let
        clearScreen =
            C.shapes
                [ CSettings.fill (Color.rgba 0 0 0 255) ]
                [ C.rect ( 0, 0 ) (toFloat state.width) (toFloat state.height) ]
    in
    {-
       C.toHtmlWith { width = width, height = height, textures = image.sources }
           (Event.eventList (EventMsg >> lifter) state)
           (clearScreen :: image.toDraw ++ annotation.toDraw)
           -- MEMO: 以下のコードがないと, iOS, iPadOSでタッチイベントが正常に発火されない.
           |> UI.html
           |> UI.el [ UI.width (UI.px state.width), UI.height (UI.px state.height) ]
           |> UI.layout []
    -}
    C.toHtmlWith { width = width, height = height, textures = image.sources }
        []
        (clearScreen :: image.toDraw ++ annotation.toDraw)
        -- MEMO: 以下のコードがないと, iOS, iPadOSでタッチイベントが正常に発火されない.
        |> UI.html
        |> UI.el
            ([ UI.width (UI.px state.width), UI.height (UI.px state.height) ]
                ++ List.map UI.htmlAttribute (Event.eventList (EventMsg >> lifter) state)
            )
        |> UI.layout []


{-| tick
-}
tick : Posix -> Msg
tick =
    OnAnimationFrame


{-| getMode
-}
getMode : Makie msg -> Mode
getMode (M.Makie state) =
    state.mode


{-| setMode
-}
setMode : Mode -> Makie msg -> Makie msg
setMode mode (M.Makie state) =
    { state | mode = mode } |> M.Makie



-- Privates


extractParams : { options : Options, project : Project, data : Data, lifter : Msg -> msg } -> S.Params
extractParams ps =
    let
        f (M.Options opt) =
            opt

        g (M.Project prj) =
            prj
    in
    { options = f ps.options, project = g ps.project, data = M.getData ps.data }


reflect : S.Params -> (Msg -> msg) -> Posix -> S.State msg -> S.State msg
reflect params lifter currentTime ({ image, annotation } as state) =
    case List.isEmpty state.cameraActions of
        True ->
            let
                updateImage s =
                    if Pathology.isSameStatus (Pathology.getStatus image.pathology) image.pathologyStatus then
                        s

                    else
                        renderImage params lifter s

                updateAnnotation s =
                    {-
                       TODO: ここを治す AnnotationにもgetStatus必要か?
                          if annotation.replicaStatus == params.data.status then
                              s

                          else
                    -}
                    renderAnnotation params lifter s
            in
            state |> updateImage |> updateAnnotation

        False ->
            { state
                | camera = Camera.update state.cameraActions state.camera
                , cameraActions = []
            }
                |> renderImage params lifter
                |> renderAnnotation params lifter


renderImage : S.Params -> (Msg -> msg) -> S.State msg -> S.State msg
renderImage _ lifter ({ image } as state) =
    let
        pathologyMsg : (Maybe Texture -> Pathology.Msg) -> (Maybe Texture -> msg)
        pathologyMsg f =
            f >> PathologyMsg >> lifter

        renderedImage : S.ImageState msg
        renderedImage =
            let
                rendered =
                    Pathology.render image.pathology state.camera
            in
            { image
                | toDraw = rendered.toDraw
                , sources =
                    List.map
                        (\s -> Texture.loadFromImageUrl s.url (pathologyMsg s.message))
                        rendered.sources
                , pathologyStatus = Pathology.getStatus image.pathology
            }
    in
    { state | image = renderedImage }


renderAnnotation : S.Params -> (Msg -> msg) -> S.State msg -> S.State msg
renderAnnotation params _ state =
    Annotation.render params state
