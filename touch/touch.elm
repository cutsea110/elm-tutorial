import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Signal
import Text (asText)
import Time (Time, every)
import Touch
import Window
import Temperature (kelvinToColor)

main : Signal Element
main = Signal.map3 scene Window.dimensions Touch.touches (every 1)

scene : (Int,Int) -> List Touch.Touch -> Time -> Element
scene (w,h) touches t =
    let dots = List.map (makeCircle t <| pair toFloat (w, h)) touches
    in layers [ collage w h dots, asText t ]

pair : (a -> b) -> (a, a) -> (b, b)
pair f (x, y) = (f x, f y)

makeCircle : Time -> (Float, Float) -> Touch.Touch -> Form
makeCircle now (w, h) {x, y, t0} =
    let size = 50 + (now - t0) / 100
        clr = kelvinToColor (now - t0) 0.8
    in
      ngon 5 size
          |> filled clr
          |> move (toFloat x - w/2, h/2 - toFloat y)
          |> rotate (now - t0)
