import Graphics.Element (..)
import Signal (Signal, (<~), constant)
import Http (Response(..), get, send)
import Text (asText, plainText)
import List (map)
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
main = (display << decodeString brands << result) <~ query "445"

display x = case x of
              Ok {items} -> flow down <| map asText items
              Err e -> flow down [ plainText e ]

query : String -> Signal (Response String)
query b = get ("http://localhost/api/v1.0.0/brands/?type=json&q=%25" ++ b ++ "%25")
        |> constant
        |> send
