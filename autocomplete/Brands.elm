module Brands (responseToBrands) where

import Graphics.Element (..)
import Http (Response(..))
import Json.Decode (decodeString, int, string, (:=), list, object2, object3, Decoder)
import Text (asText, plainText)
import List

type alias Item = { brandCode : String, brandName : String }
type alias Brands = { offset : Int
                    , count : Int
                    , items : List Item
                    }

responseToBrands : Response String -> Element
responseToBrands = display << decodeBrands << result

decodeBrands : String -> Result String Brands
decodeBrands = decodeString brands

result : Response String -> String
result res =
    case res of
      Success a -> a
      Waiting -> "Waiting"
      Failure n a -> "Failure " ++ toString n ++ " " ++ a

display : Result String Brands -> Element
display x = case x of
              Ok {items} -> flow down <| List.map asText items
              Err e -> flow down [ plainText e ]

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
