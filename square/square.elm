import Text (..)
import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)

main = collage 200 200 [ filled blue (square 100)
                       , filled red (square 100) |> move (toFloat 20, toFloat 20) |> rotate (toFloat 45)
                       ]
