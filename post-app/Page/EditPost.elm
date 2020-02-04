module Page.EditPost exposing ( Model, Msg, init, update, view )

import Browser.Navigation as Nav
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick )
import Json.Decode as Decode 
import RemoteData 

import Post
import Route
import Error exposing (buildErrorMessage)

-- MODEL

type alias Model = 
   { navKey : Nav.Key
   , post : RemoteData.WebData Post.Post
   , saveError : Maybe String
   }

init : Post.PostId -> Nav.Key -> ( Model, Cmd Msg )
init postId navKey = 
   ( initialModel navKey, fetchPost postId )

initialModel : Nav.Key -> Model 
initialModel navKey =
   { navKey = navKey
   , post = RemoteData.Loading
   , saveError = Nothing
   }


--UPDATE 

type Msg
   = PostReceived ( RemoteData.WebData Post.Post )
   | UpdateTitle String
   | UpdateAuthorName String
   | UpdateAuthorUrl String
   | SavePost
   | PostSaved ( Result Http.Error Post.Post )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
   case msg of 
      PostReceived post ->
         ( { model | post = post }
         , Cmd.none 
         )
      
      UpdateTitle newTitle ->
         let
            updateTitle = 
               RemoteData.map
                  (\postData ->
                     { postData | title = newTitle }
                  )
                  model.post
         in 
            ( {model | post = updateTitle}, Cmd.none )
      
      UpdateAuthorName newAuthorName ->
         let
            updateAuthorName =
               RemoteData.map 
                  (\postData ->
                     {postData | authorName = newAuthorName }
                  )
                  model.post
         in
            ( { model | post = updateAuthorName }, Cmd.none )
      
      UpdateAuthorUrl newAuthorUrl ->
         let
             updateAuthorUrl =
               RemoteData.map
                  (\postData ->
                     { postData | authorUrl = newAuthorUrl }
                  )
                  model.post
         in
            ( { model | post = updateAuthorUrl }, Cmd.none )
      
      SavePost ->
         ( model, savePost model.post )
      
      PostSaved ( Ok postData ) ->
         let
             post =
               RemoteData.succeed postData
         in
            ( { model | post = post, saveError = Nothing } 
            , Route.pushUrl Route.Posts model.navKey
            )
      
      PostSaved ( Err error ) -> 
         ( { model | saveError = Just ( buildErrorMessage error ) }
         , Cmd.none
         )


savePost : RemoteData.WebData Post.Post -> Cmd Msg
savePost post =
   case post of 
      RemoteData.Success postData ->
         let
             postUrl = 
               "http://localhost:5019/posts/" ++ Post.idToString postData.id
         in
            Http.request
               { method = "PATCH"
               , headers = []
               , url = postUrl
               , body = Http.jsonBody ( Post.postEncoder postData )
               , expect = Http.expectJson PostSaved Post.postDecoder
               , timeout = Nothing
               , tracker = Nothing
               }
      _ ->
         Cmd.none
         


fetchPost : Post.PostId -> Cmd Msg 
fetchPost postId = 
   Http.get
      { url = "http://localhost:5019/posts/" ++ Post.idToString postId
      , expect = 
         Post.postDecoder
            |> Http.expectJson ( RemoteData.fromResult >> PostReceived )

      }

-- VIEW
view : Model -> Html Msg
view  model =
   div [] 
      [ h3 [] [ text "Edit Post" ]
      , viewPost model.post
      , viewSaveError model.saveError
      ]

viewPost : RemoteData.WebData Post.Post -> Html Msg
viewPost post =
   case post of
      RemoteData.NotAsked ->
         text ""
      
      RemoteData.Loading ->
         h3 [] [ text "Loading Post..." ]
      
      RemoteData.Success postData ->
         editForm postData
      
      RemoteData.Failure httpError ->
         viewFetchError ( buildErrorMessage  httpError )

editForm : Post.Post -> Html Msg
editForm post =
   Html.form [] 
      [ div []
         [ text  "Title"
         , br [] []
         , input 
            [ type_ "text"
            , value post.title
            , onInput UpdateTitle
            ] []
         ]
      , br [] []
      , div []
         [ text "Author Name"
         , br [] []
         , input
            [ type_ "text"
            , value post.authorName
            , onInput UpdateAuthorName
            ] []
         ]
      , br [] []
      , div []
         [ text "Author URL"
         , br [] []
         , input
            [ type_ "text"
            , value post.authorUrl
            , onInput UpdateAuthorUrl
            ] []
         ]
      , br [] []
      , div []
         [ button [ type_ "button", onClick SavePost ]
            [ text "Submit" ]
         ]
      ]

viewFetchError : String -> Html Msg
viewFetchError errorMessage =
   let
       errorHeading =
         "Couldn't fetch post at this time."    
   in
      div []
         [ h3 [] [ text errorHeading ]
         , text ( "Error: " ++ errorMessage )
         ]

viewSaveError : Maybe String -> Html msg
viewSaveError maybeError = 
   case maybeError of
      Just error ->
         div []
            [ h3 [] [ text "Couldn't save post at this time" ]
            , text ( "Error : " ++ error )
            ]
      
      Nothing ->
         text ""