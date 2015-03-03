module App where

import Graphics.Element (..)
import Graphics.Input.Field (..)
import Signal (..)
import String
import List
import Text (plainText)
import Debug

type alias CandidateList = List String
type alias Model = { content : Content
                   , candidateList : CandidateList
                   }

initModel : Model
initModel = { content = noContent, candidateList = [] }

appChannel : Channel Content
appChannel = channel noContent

data : List String
data = [ "1111"
       , "1123"
       , "1234"
       , "1345"
       , "1678"
       ]

model : Signal Model
model = foldp update initModel (subscribe appChannel)

update : Content -> Model -> Model
update content model = {model| content <- content
                       , candidateList <- List.filter (String.contains content.string) data }

main : Signal Element
main = display << Debug.watch "model:" <~ model

display : Model -> Element
display {content, candidateList} =
    (field defaultStyle (send appChannel) "Code" content)
    `above`
    (flow down <| List.map plainText candidateList)
