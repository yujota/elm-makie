module Makie.Internal.Canvas exposing
    ( clearRequest
    , initDisplay
    , render
    , requestRenderAnnotations
    , requestRenderEditing
    , requestRenderImages
    , requestRendering
    , textureLoaded
    )

import Angle
import Canvas as C
import Canvas.Settings as CSettings
import Canvas.Settings.Advanced as CAdvanced
import Canvas.Settings.Line as CLine
import Canvas.Texture exposing (Texture)
import Color exposing (Color)
import Makie.Internal.Annotations as Annotations
import Makie.Internal.Labels as Labels
import Makie.Internal.Makie as M
import Pixels
import Point2d
import Quantity
import Rectangle2d


initDisplay : { src : String } -> M.Display
initDisplay { src } =
    { texture = Nothing, src = src, images = [], annotations = [], editing = [], request = M.NotNecessary }


render : M.MakieRecord -> M.MakieRecord
render ({ display } as m) =
    renderLoop display.request m |> clearRequest


renderLoop : M.RenderRequest -> M.MakieRecord -> M.MakieRecord
renderLoop requestType ({ camera, target, display, categories } as m) =
    let
        update d =
            { m | display = d }
    in
    case requestType of
        M.NotNecessary ->
            m

        M.RenderImages ->
            case display.texture of
                Just txt ->
                    { display | images = renderImage camera txt |> List.singleton } |> update

                Nothing ->
                    m

        M.RenderEditing ->
            let
                rdr { label, notices, handle } =
                    { display
                        | editing =
                            { label = label, notices = notices, shape = Annotations.shapeTo handle }
                                |> renderAnnotation True camera categories
                    }
                        |> update
            in
            case target of
                M.TargetCreating r ->
                    rdr r

                M.TargetEditing _ r ->
                    rdr r

                _ ->
                    { display | editing = [] } |> update

        M.RenderAnnotations ->
            renderAnnotations m |> update

        M.RenderAll ->
            m |> renderLoop M.RenderImages |> renderLoop M.RenderAnnotations |> renderLoop M.RenderEditing


renderAnnotations : M.MakieRecord -> M.Display
renderAnnotations ({ display, annotations, paneWidth, paneHeight, camera, categories, target } as m) =
    let
        _ =
            Debug.log "renderAnnotations iscalled" target

        rdr =
            case target of
                M.NoTarget ->
                    Tuple.second >> renderAnnotation False camera categories

                M.TargetCreating _ ->
                    Tuple.second >> renderAnnotation False camera categories

                M.TargetSelected key _ ->
                    \( k, a ) ->
                        if k == key then
                            let
                                _ =
                                    Debug.log "selected" ( k, a )
                            in
                            renderAnnotation True camera categories a

                        else
                            let
                                _ =
                                    Debug.log "not selected" ( k, a )
                            in
                            renderAnnotation False camera categories a

                M.TargetEditing key _ ->
                    \( k, a ) ->
                        if k == key then
                            []

                        else
                            renderAnnotation False camera categories a
    in
    Annotations.visibleItems { paneWidth = toFloat paneWidth, paneHeight = toFloat paneHeight } camera annotations
        |> List.concatMap rdr
        |> (\l -> { display | annotations = l })


textureLoaded : Maybe Texture -> M.MakieRecord -> M.MakieRecord
textureLoaded maybeTexture ({ display } as m) =
    case maybeTexture of
        Just txt ->
            { display | texture = Just txt } |> (\d -> { m | display = d }) |> requestRenderImages

        Nothing ->
            m


requestRendering : M.MakieRecord -> M.MakieRecord
requestRendering ({ display } as m) =
    { m | display = { display | request = M.RenderAll } }


requestRenderImages : M.MakieRecord -> M.MakieRecord
requestRenderImages ({ display } as m) =
    case display.request of
        M.NotNecessary ->
            { m | display = { display | request = M.RenderImages } }

        M.RenderImages ->
            m

        _ ->
            { m | display = { display | request = M.RenderAll } }


requestRenderAnnotations : M.MakieRecord -> M.MakieRecord
requestRenderAnnotations ({ display } as m) =
    case display.request of
        M.NotNecessary ->
            { m | display = { display | request = M.RenderAnnotations } }

        M.RenderAnnotations ->
            m

        _ ->
            { m | display = { display | request = M.RenderAll } }


requestRenderEditing : M.MakieRecord -> M.MakieRecord
requestRenderEditing ({ display } as m) =
    case display.request of
        M.NotNecessary ->
            { m | display = { display | request = M.RenderEditing } }

        M.RenderEditing ->
            m

        _ ->
            { m | display = { display | request = M.RenderAll } }


clearRequest : M.MakieRecord -> M.MakieRecord
clearRequest ({ display } as m) =
    { m | display = { display | request = M.NotNecessary } }



-- Helpers
-- Textures


renderImage : M.CameraRecord -> Texture -> C.Renderable
renderImage camera t =
    let
        ( x, y ) =
            Point2d.origin
                |> M.toPanePoint camera.reductionRate camera.imageFrame
                |> Point2d.toTuple Pixels.inPixels

        degrees =
            camera.angle |> Angle.inRadians

        scale =
            Quantity.ratio camera.reductionRate (M.reductionRate 1)
    in
    C.texture
        [ CAdvanced.transform
            [ CAdvanced.translate x y
            , CAdvanced.scale scale scale
            , CAdvanced.rotate degrees
            ]
        ]
        ( 0, 0 )
        t



-- Annotations


renderAnnotation : Bool -> M.CameraRecord -> M.Categories -> M.AnnotationRecord -> List C.Renderable
renderAnnotation isSelected camera categories { label, notices, shape } =
    Maybe.andThen (Labels.getColors categories) label
        |> Maybe.map
            (\colors ->
                case ( isSelected, shape ) of
                    ( True, M.Point pointShape ) ->
                        renderPointAnnotation
                            { color = colors.selectedLineColor, radius = predefinedSettings.paRadiusSelected }
                            camera
                            pointShape

                    ( False, M.Point pointShape ) ->
                        renderPointAnnotation
                            { color = colors.lineColor, radius = predefinedSettings.paRadius }
                            camera
                            pointShape

                    ( True, M.Rectangle rectangleShape ) ->
                        renderRectangleAnnotation
                            { lineColor = colors.selectedLineColor
                            , lineWidth = predefinedSettings.raLineWidth
                            , fillColor = colors.selectedFillColor
                            , cornerPointRadius = predefinedSettings.raCornerPointRadius
                            , displayCornerPoint = True
                            }
                            camera
                            rectangleShape

                    ( False, M.Rectangle rectangleShape ) ->
                        renderRectangleAnnotation
                            { lineColor = colors.lineColor
                            , lineWidth = predefinedSettings.raLineWidth
                            , fillColor = colors.fillColor
                            , cornerPointRadius = 0
                            , displayCornerPoint = False
                            }
                            camera
                            rectangleShape

                    ( _, M.Polygon polygonShape ) ->
                        -- TODO
                        []
            )
        |> Maybe.withDefault []


renderPointAnnotation : { color : Color, radius : Float } -> M.CameraRecord -> M.PointShape -> List C.Renderable
renderPointAnnotation { color, radius } camera { point } =
    let
        { x, y } =
            M.toPanePoint camera.reductionRate camera.imageFrame point |> Point2d.toPixels
    in
    C.shapes [ CSettings.fill color ] [ C.circle ( x, y ) radius ] |> List.singleton


renderRectangleAnnotation :
    { lineColor : Color, lineWidth : Float, fillColor : Color, cornerPointRadius : Float, displayCornerPoint : Bool }
    -> M.CameraRecord
    -> M.RectangleShape
    -> List C.Renderable
renderRectangleAnnotation opt camera { rectangle } =
    let
        paneRectangle =
            Rectangle2d.at_ camera.reductionRate rectangle |> Rectangle2d.placeIn camera.imageFrame

        vertices =
            Rectangle2d.vertices paneRectangle |> List.map (Point2d.toTuple Pixels.inPixels)

        topLeft =
            List.head vertices |> Maybe.withDefault ( 0, 0 )

        ( width, height ) =
            Rectangle2d.dimensions paneRectangle |> Tuple.mapBoth Pixels.inPixels Pixels.inPixels

        rect =
            C.shapes
                [ CSettings.fill opt.fillColor
                , CSettings.stroke opt.lineColor
                , CLine.lineWidth opt.lineWidth
                , CAdvanced.transform [ CAdvanced.rotate (Angle.inRadians camera.angle) ]
                ]
                [ C.rect topLeft width height ]

        cornerPoint t =
            C.shapes [ CSettings.fill opt.lineColor ] [ C.circle t opt.cornerPointRadius ]
    in
    (\l -> l ++ [ rect ]) <|
        if opt.displayCornerPoint then
            List.map cornerPoint vertices

        else
            []


predefinedSettings =
    { raLineWidth = 3, raCornerPointRadius = 3, paRadius = 3, paRadiusSelected = 5 }
