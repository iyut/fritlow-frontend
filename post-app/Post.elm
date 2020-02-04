module Post exposing 
   ( Post
   , PostId
   , idToString
   , postDecoder
   , postsDecoder
   , postEncoder
   , idParser
   )

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Url.Parser as Parser

type PostId =
   PostId Int

type alias Post =
   { id : PostId
   , title : String
   , authorName : String 
   , authorUrl : String 
   }

postsDecoder : Decode.Decoder (List Post) 
postsDecoder = 
   Decode.list postDecoder

postDecoder : Decode.Decoder Post
postDecoder = 
   Decode.succeed Post
      |> DecodePipeline.required "id" idDecoder
      |> DecodePipeline.required "title" Decode.string
      |> DecodePipeline.required "authorName" Decode.string
      |> DecodePipeline.required "authorUrl" Decode.string 

postEncoder : Post -> Encode.Value
postEncoder post = 
   Encode.object
      [ ( "id", encodeId post.id )
      , ( "title", Encode.string post.title )
      , ( "authorName", Encode.string post.authorName )
      , ( "authorUrl", Encode.string post.authorUrl )
      ]

encodeId : PostId -> Encode.Value
encodeId (PostId id) = 
   Encode.int id

idDecoder : Decode.Decoder PostId
idDecoder = 
   Decode.map PostId Decode.int

idToString : PostId -> String
idToString postId =
   case postId of
      PostId id ->
         String.fromInt id

idParser : Parser.Parser (PostId -> a) a
idParser = 
   Parser.custom "POSTID" <|
      \postId ->
         Maybe.map PostId ( String.toInt postId )