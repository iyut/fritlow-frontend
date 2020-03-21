module Page.Shop exposing ( Model, Msg )

import Html exposing (..)
import Html.Attributes exposing (.. )
import Http
import Json.Decode as Decode 
import RemoteData

import Product
import Error

-- MODEL

type alias Model =
    { products : RemoteData.WebData ( List Product.Product )
    , deleteError : Maybe String
    }

type Msg 
    = FetchProducts
    | ProductsReceived ( RemoteData.WebData ( List Product.Product ) )