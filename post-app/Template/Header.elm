module Template.Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL
type alias Model =
   { activeMenu : String
   , logoUrl : String 
   , menus : List Menu
   }

type alias Menu = 
   { title : String
   , link : String
   , slug : String 
   , parent : String
   }

init : ( Model, Cmd Msg )
init = (
   { activeMenu = ""
   , logoUrl = ""
   , menus = 
      [  { title = "Home"
         , link = "/home"
         , slug = "home"
         , parent = ""
         }
      ,  { title = "About"
         , link = "/about"
         , slug = "about"
         , parent = ""
         }
      ]
   }, Cmd.none )

-- UPDATE 
type Msg = 
   MenuClicked

--VIEW
view : Model -> Html Msg
view model = 
   div [ class "header-container", id "header-container" ] 
      [ viewLogo model
      , viewNav model
      ]

viewLogo : Model -> Html msg 
viewLogo model = 
   div [ class "logo-container", id "logo-container" ]
      [ a [ href "#", class "logo-link" ] 
         [ img [ src model.logoUrl, class "logo-img" ] []
         ]
      ]

viewNav : Model -> Html msg 
viewNav model =
   div [ class "nav-container", id "nav-container" ]
      [ a [ href "#", class "nav-toggle" ]
         [ span [] []
         ]
      , viewMenu model
      ]

viewMenu : Model -> Html msg 
viewMenu model = 
   div [ class "nav-menu", id "nav-menu"] 
      [ ul [ class "nav-menu-list", id "nav-menu-list" ] 
         [ 

         ]

      ]

viewMenuList : Menu -> Html msg 
viewMenuList menu = 
   let
       liClass = "menu-" ++ menu.slug
       linkClass = "menu-link-" ++ menu.slug
   in
   
   li [ class liClass ] 
      [ a [ href menu.link, class linkClass ]
         [ text menu.title ]
      ]