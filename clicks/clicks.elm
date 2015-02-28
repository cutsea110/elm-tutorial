import Mouse
import Signal(..)
import Text(asText)
import Graphics.Element(Element)

main : Signal Element
main = asText <~ countClick

countClick : Signal Int
countClick = foldp (\clk count -> count + 1) 0 Mouse.clicks
