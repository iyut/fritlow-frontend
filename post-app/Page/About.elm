module Page.About exposing( Model, init, view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing( onClick )

-- MODEL

type alias Model =
   { title : String
   , content : String 
   }

init : (Model, Cmd Msg )
init = ({title = "About Title", content = "About Content" }, Cmd.none)


-- UPDATE
type Msg =
   Title
   | Content 


-- VIEW

view : Model -> Html msg
view model =
   div[] 
   [ text model.content
   ]