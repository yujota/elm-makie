module Makie.Types.Data exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Makie.Types.Annotation as A


type alias Data =
    { status : String
    , image : Image
    , annotations : Dict String A.Annotation
    , history : History
    }


type alias History =
    {- TODO:  余裕があったら実装


       { initialMemory : Dict String A.Annotation
       , initialStatus : String
       , currentIndex : Maybe Int -- Undo したあとの状態, あとで Redoするため
       , actions : Array ( String, List HistoryOperation ) -- 古いほうが左, Nothing の場合は削除
       , maxStep : Int
       }
    -}
    {}


type HistoryOperation
    = Change String A.Annotation
    | Remove String


type Image
    = NormalImage
        { identifier : ImageIdentifier
        , name : String
        , url : String
        , width : Int
        , height : Int
        }
    | PathologyImage
        { identifier : ImageIdentifier
        , name : String
        , url : { x : Int, y : Int, z : Int } -> Maybe String
        , width : Int -- Lv0 Width
        , height : Int -- Lv0 Height
        , tileWidth : Float
        , tileHeight : Float
        , layers : List { reductionRate : Float, width : Float, height : Float }
        }


type ImageIdentifier
    = UserSpecifiedId String
    | OpenSlideQuickHash1 String
