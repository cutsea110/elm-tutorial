import Graphics.Element(..)
import Mouse
import Signal(..)
import Text(asText)

main : Signal Element
main = asText <~ Mouse.isDown

