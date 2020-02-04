module Page.ListPosts exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import RemoteData

import Post
import Error



-- MODEL

type alias Model =
    { posts : RemoteData.WebData (List Post.Post)
    , deleteError : Maybe String
    }


type Msg
    = FetchPosts
    | PostsReceived ( RemoteData.WebData (List Post.Post))
    | DeletePost Post.PostId
    | PostDeleted ( Result Http.Error String )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchPosts )

initialModel : Model
initialModel = 
   { posts = RemoteData.Loading
   , deleteError = Nothing 
   }

-- UPDATE 

fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts/"
        , expect =
            Post.postsDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostsReceived)
        }

deletePost : Post.PostId -> Cmd Msg 
deletePost postId =
   Http.request
      { method = "DELETE"
      , headers = []
      , url = "http://localhost:5019/posts/" ++ Post.idToString postId
      , body = Http.emptyBody
      , expect = Http.expectString PostDeleted
      , timeout = Nothing
      , tracker = Nothing  
      }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
         FetchPosts ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

         PostsReceived response ->
            ( { model | posts = response }, Cmd.none )
         
         DeletePost postId -> 
            ( model, deletePost postId )
         
         PostDeleted ( Ok _ ) ->
            ( model, fetchPosts )
         
         PostDeleted ( Err error ) -> 
            ( { model | deleteError = Just ( Error.buildErrorMessage error ) }
            , Cmd.none
            )



-- VIEWS


view : Model -> Html Msg
view model =
   div []
      [ button [ onClick FetchPosts ]
         [ text "Refresh posts" ]
      , br [] []
      , br [] []
      , a [ href "/posts/new" ]
         [ text "Add New Post" ]
      , viewPosts model.posts
      , viewDeleteError model.deleteError
   ]


viewPosts : RemoteData.WebData (List Post.Post) -> Html Msg
viewPosts posts =
    case posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualPosts ->
            div []
                [ h3 [] [ text "Posts" ]
                , table []
                    ([ viewTableHeader ] ++ List.map viewPost actualPosts)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (Error.buildErrorMessage httpError)


viewTableHeader : Html Msg
viewTableHeader =
   tr []
      [ th []
         [ text "ID" ]
      , th []
         [ text "Title" ]
      , th []
         [ text "Author" ]
      , th [] [ text "Action" ]
      , th [] [ text "" ]
      ]


viewPost : Post.Post -> Html Msg
viewPost post =
   let 
      postPath = 
         "/posts/" ++ Post.idToString post.id 
   in 
      tr []
        [ td []
            [ text (Post.idToString post.id) ]
        , td []
            [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
         , td []
            [ a [ href postPath ] [ text "edit" ] ]
         , td []
            [ button [ type_ "button", onClick ( DeletePost post.id ) ] [ text "Delete" ]
            ]
      ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch posts at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]

viewDeleteError : Maybe String -> Html msg
viewDeleteError maybeError = 
   case maybeError of 
      Just error ->
         div []
            [ h3 [] [ text "Couldn't delete the post this time." ]
            , text ( "Error: " ++ error )
            ]
      
      Nothing ->
         text ""