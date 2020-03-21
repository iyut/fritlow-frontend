module Product exposing
    ( Product
    , ProductId
    )

type ProductId =
   ProductId Int

type alias Product =
   { id : ProductId
   , title : String
   , authorName : String 
   , authorUrl : String 
   }