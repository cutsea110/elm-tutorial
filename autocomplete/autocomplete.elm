module App where

import Color (..)
import Dict
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Input.Field (..)
import Signal (..)
import Http (Request, Response)
import Http
import Text (asText)
import List
import Maybe (withDefault)
import Keyboard (..)
import Char
import String
import Touch
import Window

import Brands (..)

main : Signal Element
main = view <~ lastCode ~ codeField ~ (responseToBrands <~ query) ~ draw

query : Signal (Response String)
query = Http.send <| httpGet << .string <~ subscribe code

httpGet : String -> Request String
httpGet b = let b' = if String.isEmpty b then "dummy" else b
            in Http.get ("http://192.168.10.107/api/v1.0.0/brands/?type=json&q=" ++ b')

code : Channel Content
code = channel noContent

codeField : Signal Element
codeField = field defaultStyle (send code) "Code" <~ subscribe code

lastCode : Signal Element
lastCode = asText << Char.fromCode <~ lastPressed

view : Element -> Element -> Element -> Element -> Element
view key c codes d = layers [d, List.foldr above empty [c, codes]]

draw : Signal Element
draw = scene <~ Window.dimensions ~ (map Dict.values (foldp addN Dict.empty Touch.touches))


addN : List Touch.Touch -> Dict.Dict Int (List (Int,Int)) -> Dict.Dict Int (List (Int,Int))
addN touches dict =
  List.foldl add1 dict touches


add1 : Touch.Touch -> Dict.Dict Int (List (Int,Int)) -> Dict.Dict Int (List (Int,Int))
add1 touch dict =
  let oldPoints = withDefault [] (Dict.get touch.id dict)
      newPoint = (touch.x, touch.y)
  in
      Dict.insert touch.id (newPoint :: oldPoints) dict


scene : (Int,Int) -> List (List (Int,Int)) -> Element
scene (w,h) paths =
  let float (a,b) = (toFloat a, toFloat -b)
      pathForms = group (List.map (traced thickLine << path << List.map float) paths)
      picture = collage w h [ move (float (-w // 2, -h // 2)) pathForms ]
  in
      layers [ picture ]


thickLine : LineStyle
thickLine =
  { defaultLine |
      color <- rgba 123 123 123 0.3,
      width <- 8
  }
