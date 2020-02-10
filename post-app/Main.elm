module Main exposing (main) 

import Browser
import Browser.Navigation as Nav
import Html exposing (..)

import Template.Header as Header 

import Page.About as About
import Page.ListPosts as ListPosts
import Page.EditPost as EditPost
import Page.NewPost as NewPost
import Post
import Route
import Url



-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL

type alias Model =
    { route : Route.Route
    , page : Page
    , navKey : Nav.Key
    }

type Template
    = Header Header.Model

type Page
    = NotFoundPage
    | HomePage
    | AboutPage About.Model
    | ListPage ListPosts.Model
    | EditPage EditPost.Model
    | NewPage NewPost.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = HomePage
            , navKey = navKey
            }
    in
        initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )
                
                Route.Home ->
                    ( HomePage, Cmd.none )
                
                Route.About ->
                    let
                        ( pageModel, pageCmds ) = 
                            About.init    
                    in
                        ( AboutPage pageModel, Cmd.none )

                Route.Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init
                    in
                        ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )
                
                Route.Post postId ->
                    let
                        ( pageModel, pageCmd ) =
                            EditPost.init postId model.navKey
                    in
                        ( EditPage pageModel, Cmd.map EditPageMsg pageCmd )
                
                Route.NewPost ->
                    let
                        ( pageModel, pageCmd ) =
                            NewPost.init model.navKey
                    in
                        ( NewPage pageModel, Cmd.map NewPageMsg pageCmd )
                  
                  
   in
      ( { model | page = currentPage }
      , Cmd.batch [ existingCmds, mappedPageCmds ]
      )




-- UPDATE 

type Msg
   = ListPageMsg ListPosts.Msg
   | LinkClicked Browser.UrlRequest
   | UrlChanged Url.Url
   | EditPageMsg EditPost.Msg
   | NewPageMsg NewPost.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
         ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListPosts.update subMsg pageModel
            in
                ( { model | page = ListPage updatedPageModel }
                , Cmd.map ListPageMsg updatedCmd
                )

         ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

         ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
               ( { model | route = newRoute }, Cmd.none )
                  |> initCurrentPage
         
         ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
               ( updatedPageModel, updatedCmd ) =
                  EditPost.update subMsg pageModel
            in
               ( { model | page = EditPage updatedPageModel }
               , Cmd.map EditPageMsg updatedCmd
               )
         
         ( NewPageMsg subMsg, NewPage pageModel ) ->
            let
               ( updatedPageModel, updatedCmd ) = 
                  NewPost.update subMsg pageModel    
            in
               ( { model | page = NewPage updatedPageModel }
               , Cmd.map NewPageMsg updatedCmd
               )

         ( _, _ ) ->
            ( model, Cmd.none )




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = 
   Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        headerModel = 
            { route = model.route
            , navKey = model.navKey
            }
    in
    
    { title = "Post App"
    , body = 
      [ Header.view headerModel 
      , currentView model
      ]
    }


currentView : Model -> Html Msg
currentView model =
   case model.page of
      NotFoundPage ->
         notFoundView

      HomePage ->
         homeView 
      
      AboutPage pageModel ->
         About.view pageModel

      ListPage pageModel ->
         ListPosts.view pageModel
            |> Html.map ListPageMsg
      
      EditPage pageModel ->
         EditPost.view pageModel
            |> Html.map EditPageMsg
      
      NewPage pageModel ->
         NewPost.view pageModel
            |> Html.map NewPageMsg
      


homeView : Html msg
homeView = 
   div [] [ text "this is home" ]

notFoundView : Html msg
notFoundView =
   h3 [] [ text "Oops! The page you requested was not found!" ]