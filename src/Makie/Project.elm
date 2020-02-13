module Makie.Project exposing
    ( Project, Categories
    , project, empty
    , selected, setSelected, categories, setCategories
    )

{-| This module provides API related to Project


# Types

@docs Project, Categories


# Constructor

@docs project, empty


# Getter and setter

@docs selected, setSelected, categories, setCategories

-}

import Dict exposing (Dict)
import Makie.Category as Category exposing (Category)
import Makie.Internal.Makie as M


{-| Project type.
-}
type alias Project =
    M.Project


{-| Categories type.
-}
type alias Categories =
    List ( Category.Id, Category )


{-| Create project
-}
project : String -> Categories -> Maybe Category.Id -> Project
project name categoryList maybeCategoryId =
    { name = name
    , selectedCategory = Nothing
    , categories = Dict.empty
    }
        |> M.Project
        |> setCategories categoryList
        |> setSelected maybeCategoryId


{-| Empty project
-}
empty : Project
empty =
    { name = ""
    , selectedCategory = Nothing
    , categories = Dict.empty
    }
        |> M.Project


{-| Get selected category.
-}
selected : Project -> Maybe ( Category.Id, Category )
selected (M.Project prj) =
    prj.selectedCategory
        |> Maybe.andThen (\k -> Maybe.map (Tuple.pair k) (Dict.get k prj.categories))
        |> Maybe.map (Tuple.mapBoth M.CategoryId M.Category)


{-| Set selected category.
-}
setSelected : Maybe Category.Id -> Project -> Project
setSelected maybeCategoryId (M.Project prj) =
    M.Project <|
        case maybeCategoryId of
            Just (M.CategoryId catId) ->
                if Dict.member catId prj.categories then
                    { prj | selectedCategory = Just catId }

                else
                    prj

            Nothing ->
                { prj | selectedCategory = Nothing }


{-| Get categories
-}
categories : Project -> Categories
categories (M.Project prj) =
    prj.categories
        |> Dict.toList
        |> List.map (Tuple.mapBoth M.CategoryId M.Category)


{-| Set categories
-}
setCategories : Categories -> Project -> Project
setCategories categoryList (M.Project prj) =
    let
        extract ( M.CategoryId i, M.Category c ) =
            ( i, c )
    in
    categoryList
        |> List.map extract
        |> Dict.fromList
        |> (\l -> { prj | categories = l })
        |> M.Project
