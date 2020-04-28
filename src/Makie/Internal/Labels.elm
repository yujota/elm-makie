module Makie.Internal.Labels exposing (emptyCategories, getColors)

import Makie.Internal.Makie as M


getColors : M.Categories -> M.Label -> Maybe M.LabelColors
getColors categories label =
    case label of
        M.BelongsToCategory _ ->
            Nothing

        M.CustomLabel { colors } ->
            Just colors


emptyCategories : M.Categories
emptyCategories =
    {}
