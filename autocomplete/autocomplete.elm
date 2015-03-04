module App where

import Graphics.Element (..)
import Graphics.Input.Field (..)
import Signal (..)
import Http (Request, Response(..))
import Http
import Text (asText, plainText)
import List
import Json.Decode (decodeString, int, string, (:=), list, object2, object3, Decoder)

type alias Item = { brandCode : String, brandName : String }
type alias Brands = { offset : Int
                    , count : Int
                    , items : List Item
                    }

brands : Decoder Brands
brands = object3 Brands
      ("offset" := int)
      ("count" := int)
      ("items" := items)

items : Decoder (List Item)
items = list item

item : Decoder Item
item = object2 Item
        ("brandCode" := string)
        ("brandName" := string)

result res =
    case res of
      Success a -> a
      Waiting -> "Waiting"
      Failure n a -> "Failure " ++ toString n ++ " " ++ a

main : Signal Element
main = view <~ query ~ codeField -- (display << decodeString brands << result) <~ query
       
display x = case x of
              Ok {items} -> flow down <| List.map asText items
              Err e -> flow down [ plainText e ]

query : Signal (Response String)
query = Http.send <| httpGet <~ (.string <~ subscribe code)

httpGet : String -> Request String
httpGet b = Http.get ("http://localhost/api/v1.0.0/brands/?type=json&q=" ++ b)

code : Channel Content
code = channel noContent

codeField : Signal Element
codeField = field defaultStyle (send code) "Code" <~ subscribe code

view : Response String -> Element -> Element
view res c = c `above` (display (decodeString brands (result res)))