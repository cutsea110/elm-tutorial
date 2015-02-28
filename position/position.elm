import Mouse
import Signal(..)
import Text(asText)
import Graphics.Element(Element)

main : Signal Element
main = asText <~ Mouse.position

