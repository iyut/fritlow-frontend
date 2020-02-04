module Page.NewPost exposing ( Model, Msg, init, update, view )

import Browser.Navigation as Nav 
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing( onInput, onClick )
import Http

import RemoteData

import Post
import Error


-- MODEL 

type alias Model = 
   { navKey : Nav.Key 
   , post : Post.Post
   , createError : Maybe String
   }

init : Nav.Key -> ( Model, Cmd Msg )
init navKey = 
   ( initialModel navKey, Cmd.none )

initialModel : Nav.Key -> Model 
initialModel navKey = 
   { navKey = navKey
   , post = Post.emptyPost
   , createError = Nothing
   }

-- UPDATE

type Msg
   = StoreTitle String
   | StoreAuthorName String
   | StoreAuthorUrl String
   | CreatePost
   | PostCreated ( Result Http.Error Post.Post )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
   case msg of
      StoreTitle title ->
         let
            oldPost = 
               model.post

            updateTitle =
               { oldPost | title = title }    
         in
            ( {model | post = updateTitle }, Cmd.none )
      
      StoreAuthorName name ->
         let
            oldPost = 
               model.post 

            updateAuthorName = 
               { oldPost | authorName = name }
         in
            ( { model | post = updateAuthorName }, Cmd.none )
      
      StoreAuthorUrl url ->
         let
            oldPost = 
               model.post
            
            updateAuthorUrl = 
               { oldPost | authorUrl = url }

         in
            ( { model | post = updateAuthorUrl }, Cmd.none )

      CreatePost ->
         ( model, createPost model.post )
      
      PostCreated ( Ok post ) ->
         ( { model | post = post, createError = Nothing }, Cmd.none )
      
      PostCreated ( Err error ) -> 
         ( { model | createError = Just ( Error.buildErrorMessage error )}
         , Cmd.none
         )



createPost : Post.Post -> Cmd Msg
createPost post = 
   Http.post
      { url = "http://localhost:5019/posts"
      , body = Http.jsonBody ( Post.newPostEncoder post )
      , expect = Http.expectJson PostCreated Post.postDecoder
      }
         
         

-- VIEW
view : Model -> Html Msg
view model = 
   div []
      [ h3 [] [ text "Add New Post" ]
      , newPostForm
      , viewError model.createError
      ]

newPostForm : Html Msg
newPostForm = 
   Html.form []
      [ div []
         [ text "Title"
         , br [] []
         , input [ type_ "text", onInput StoreTitle ] []
         ]
      , br [] []
      , div []
         [ text "Author Name"
         , br [] []
         , input [ type_ "text", onInput StoreAuthorName ] []
         ]
      , br [] []
      , div []
         [ text "Author URL"
         , br [] []
         , input [ type_ "text", onInput StoreAuthorUrl ] []
         ]
      , br [] []
      , div [] 
         [ button [ type_ "button", onClick CreatePost ]
            [ text "Submit" ]
         ]
      ]

viewError : Maybe String -> Html Msg
viewError maybeError = 
   case maybeError of
      Just error ->
         div []
            [ h3 [] [ text "Couldn't create a post this time" ]
            , text ( "Error : " ++ error )
            ]
      
      Nothing ->
         text ""