import Graphics.Element (..)
import Mouse
import Signal (..)
import Text (asText)
import Time (fpsWhen)

main : Signal Element
main = asText <~ (foldp (+) 0 (30 `fpsWhen` Mouse.isDown))

