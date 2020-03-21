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

logoUrl : String
logoUrl = ""

type alias Menu = 
   { title : String
   , link : String
   , slug : String 
   , parent : String
   , active : Bool
   }

makeMenuActive : Menu -> Menu
makeMenuActive menu = 
    { menu | active = True }

menuHome : Menu 
menuHome = 
    { title = "Home"
    , link = "/"
    , slug = "home"
    , parent = ""
    , active = False
    }

menuAbout : Menu
menuAbout = 
    { title = "About"
    , link = "/about"
    , slug = "about"
    , parent = ""
    , active = False
    }

menuPosts : Menu
menuPosts =
    { title = "Posts"
    , link = "/posts"
    , slug = "posts"
    , parent = ""
    , active = False
    }

menus : List Menu
menus = 
    [ menuHome
    , menuAbout
    , menuPosts
    ]

currentMenuState : Model -> List Menu 
currentMenuState model =
    case model.route of 
        Route.NotFound ->
            menus
        
        Route.Home ->
            let
                activeMenu = 
                    makeMenuActive menuHome
            
            in 
                [ activeMenu, menuAbout, menuPosts ]
        
        Route.About ->
            let
                activeMenu = 
                    makeMenuActive menuAbout
            
            in 
                [ menuHome, activeMenu, menuPosts ]

        Route.Posts ->
            let
                activeMenu = 
                    makeMenuActive menuPosts
            
            in 
                [ menuHome, menuAbout, activeMenu ]
        
        Route.Post postId ->
            let
                activeMenu = 
                    makeMenuActive menuPosts
            
            in 
                [ menuHome, menuAbout, activeMenu ]
        
        Route.NewPost ->
            let
                activeMenu = 
                    makeMenuActive menuPosts
            
            in 
                [ menuHome, menuAbout, activeMenu ]


-- UPDATE 
type Msg = 
   MenuClicked

--VIEW
view : Model -> Html msg
view model = 
   div [ class "header-container", id "header-container" ] 
      [ viewLogo logoUrl
      , viewNav model
      , viewRightNav model
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
            ( List.map ( \x -> viewMenuList model x ) menus )
        ]


viewMenuList : Model -> Menu -> Html msg
viewMenuList model menu = 
    let
        liClass =
            if Route.routeToString model.route == menu.link || String.contains ( menu.link ++ "/" ) ( Route.routeToString model.route ) then
                "menuli menu-" ++ menu.slug ++ " " ++ "active-menu"
            else
                "menuli menu-" ++ menu.slug

        linkClass = "menu-link menu-link-" ++ menu.slug
    in
        li [ class liClass ] 
            [ a [ href menu.link, class linkClass ]
                [ text menu.title ]
            ]

viewRightNav : Model -> Html msg 
viewRightNav model = 
    div [ class "nav-mini", id "nav-mini" ]
        [ ul [ class "nav-mini-list", id "nav-mini-list" ]
            [ li [ class "nav-mini-li", id "nav-mini-li-account" ]
                [ span [ class "nav-mini-li-span" ] [ text "My Account" ] 
                ]
            , li [ class "nav-mini-li", id "nav-mini-li-wishlist" ]
                [ span [ class "nav-mini-li-span" ] [ text "Wishlist" ] 
                ]
            , li [ class "nav-mini-li", id "nav-mini-li-cart" ]
                [ span [ class "nav-mini-li-span" ] [ text "Cart" ] 
                ]
            ]
        ]