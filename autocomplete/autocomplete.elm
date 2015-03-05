module App where

import Graphics.Element (..)
import Graphics.Input.Field (..)
import Signal (..)
import Http (Request, Response)
import Http
import Text (asText)
import List
import Keyboard (..)
import Char

import Brands (..)

main : Signal Element
main = view <~ lastCode ~ codeField ~ (responseToBrands <~ query)

query : Signal (Response String)
query = Http.send <| httpGet << .string <~ subscribe code

httpGet : String -> Request String
httpGet b = Http.get ("http://localhost/api/v1.0.0/brands/?type=json&q=" ++ b)

code : Channel Content
code = channel noContent

codeField : Signal Element
codeField = field defaultStyle (send code) "Code" <~ subscribe code

lastCode : Signal Element
lastCode = asText << Char.fromCode <~ lastPressed

view : Element -> Element -> Element -> Element
view key c codes = List.foldr above empty [key, c, codes]

