module Counter exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)

-- MAIN
main : Program () Model Msg 
main = Browser.sandbox
   { init = init
   , view = view
   , update = update
   }



-- MODEL

type alias Model = Int

init : Model
init = 0




-- UPDATE

type Msg =
   Increment 
   | Decrement

update : Msg -> Model -> Model
update msg model =
   case msg of
      Increment ->
         model = model + 1
      
      Decrement ->
         model = model - 1



-- VIEW
view : Model -> Html Msg
view model = 
   div [] 
      [ button [ onClick Increment ] [ text "+" ]
      , text ( String.fromInt model )
      , button [ onClick Decrement ] [ text "-" ]
      ]