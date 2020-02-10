module Template.Header exposing ( Model, Menu, Msg, view )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)

import Route

-- MODEL

type alias Model =
    { route : Route.Route
    , navKey : Nav.Key
    }

type alias Menu = 
   { title : String
   , link : String
   , slug : String 
   , parent : String
   }

logoUrl : String
logoUrl = ""

menus : List Menu
menus = 
    [ { title = "Home"
      , link = "/"
      , slug = "home"
      , parent = ""
      }
    , { title = "About"
      , link = "/about"
      , slug = "about"
      , parent = ""
      }
    , { title = "Posts"
      , link = "/posts"
      , slug = "posts"
      , parent = ""
      }
    ]


-- UPDATE 
type Msg = 
   MenuClicked

--VIEW
view : Model -> Html msg
view model = 
   div [ class "header-container", id "header-container" ] 
      [ viewLogo logoUrl
      , viewNav model
      ]

viewLogo : String -> Html msg 
viewLogo logo = 
   div [ class "logo-container", id "logo-container" ]
      [ a [ href "#", class "logo-link" ] 
         [ img [ src logo, class "logo-img" ] []
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
         ( List.map viewMenuList menus )
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