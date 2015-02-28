import Graphics.Element (..)
import Signal (..)
import Text (asText)
import Time (fps)

main : Signal Element
main = asText <~ (foldp (+) 0 (fps 30))

