import Date (..)
import Graphics.Element (..)
import Time (every)
import Signal (..)
import String (..)
import Text (plainText)

main : Signal Element
main = plainText << toYMDHMS << fromTime <~ every 1000

toYMDHMS : Date -> String
toYMDHMS d =
    let
      ymd = toString (year d) ++ "-" ++  toString (month d) ++ "-" ++ toString (day d)
      w = "(" ++ toString (dayOfWeek d) ++ ")"
      hms = toString (hour d) ++ ":" ++ toString (minute d) ++ ":" ++ toString (second d)
    in
      ymd ++ " " ++ w ++ " " ++ hms
