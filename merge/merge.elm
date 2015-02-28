import Signal (..)
import Time (..)
import Mouse
import Graphics.Element (..)
import Text (asText)

type Update = Move (Int, Int) | TimeDelta (Float, Float)

updates : Signal Update
updates = merge (Move <~ Mouse.position) (TimeDelta  <~ timedelta)

timedelta : Signal (Float, Float)
timedelta = map2 (\b n -> (b, n)) Mouse.isDown (fps 30) |>
            foldp (\(b, n) (_, t) -> if b then (n, t+n) else (n, 0)) (0, 0)

main : Signal Element
main = asText <~ updates

