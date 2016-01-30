import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Mouse
import Signal (Signal, map3, foldp)
import Text (asText)
import Window
import Time (..)

main : Signal Element
main = map3 scene Mouse.position Window.dimensions (foldp (+) 0 (fps 30))

scene : (Int, Int) -> (Int, Int) -> Float -> Element
scene (x,y) (w,h) t =
   let (dx,dy) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
   in collage w h
        [ ngon 3 100 |> filled blue |> rotate (atan2 dy dx)
        , ngon 6 30  |> filled orange |> move (dx, dy) |> rotate t
        ]


