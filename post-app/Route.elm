module Route exposing( Route(..), parseUrl, pushUrl, routeToString )

import Browser.Navigation as Nav
import Url
import Url.Parser as Parser exposing( (</>) )
import Post 

type Route =
   NotFound
   | Home
   | About
   | Posts 
   | Post Post.PostId
   | NewPost


parseUrl : Url.Url -> Route
parseUrl url = 
   case Parser.parse matchRoute url of
      Just route ->
         route 
      
      Nothing ->
         NotFound


matchRoute : Parser.Parser (Route -> a ) a
matchRoute =
   Parser.oneOf 
      [ Parser.map Home Parser.top
      , Parser.map About ( Parser.s "about" )
      , Parser.map Posts ( Parser.s "posts" )
      , Parser.map Post ( Parser.s "posts" </> Post.idParser )
      , Parser.map NewPost ( Parser.s "posts" </> Parser.s "new" )
      ]

pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey = 
   routeToString route 
      |> Nav.pushUrl navKey

routeToString : Route -> String
routeToString route = 
   case route of 
      NotFound -> 
         "/not-found"
      
      Home ->
         "/"
      
      About ->
         "/about"
      
      Posts ->
         "/posts"
      
      Post postId -> 
         "/posts/" ++ Post.idToString postId
      
      NewPost ->
         "/posts/new"
