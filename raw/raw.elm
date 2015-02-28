import Graphics.Element (..)
import List
import Signal
import Text (asText)
import Touch


main : Signal Element
main = Signal.map (flow down << List.map asText) Touch.touches
