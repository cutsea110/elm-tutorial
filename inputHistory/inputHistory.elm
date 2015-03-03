module App where

import Text (asText)
import Html (Html, div, toElement, text, input, Attribute, li)
import Html.Attributes(class, type', value, style, id, max)
import Html.Attributes as Attr
import Html.Events(on, targetValue, onKeyDown, keyCode)
import Signal (..)
import List (tail)
import List
import String
import Array
import Array (Array)
import Time (..)
import Json.Decode as Json
import Result (..)
import Debug

type alias NameList = List String
type alias State = { input : String
                   , nameList : NameList }

type alias Model = { state: State
                   , archive : Array State
                   , current : Int }

initState : State
initState = { input = "", nameList = [] }

initModel : Model
initModel = { state = initState
            , archive = Array.fromList [initState]
            , current = 0 }

type Actions
    = None
    | Input String
    | Add String
    | Delete ()
    | ArchiveSelect Int

appChannel : Channel Actions
appChannel = channel None

model : Signal Model
model = foldp update initModel (subscribe appChannel)

update : Actions -> Model -> Model
update action model =
    let state = model.state
        write state' = let target = model.current + 1
                       in {model| state <- state'
                                , current <- target
                                , archive <- if target > ((Array.length model.archive)-1)
                                             then Array.push state' model.archive
                                             else Array.set target state' model.archive
                          }
    in case action of
      Input str -> write {state| input <- str }
      Add str -> write {state| input <- ""
                             , nameList <- state.nameList ++ [str]}
      Delete x -> if List.isEmpty state.nameList
                  then model
                  else write {state| nameList <- tail state.nameList}
      ArchiveSelect v -> {model| current <- v
                               , state <- case Array.get v model.archive of
                                            Just ste -> ste
                                            Nothing -> Debug.crash "Unexpected access"}
      _ -> model

main : Signal Html
main = view <~ model

rangeInput : Int -> Int -> Html
rangeInput max current =
    div [] [ div [] [
              text "0"
             , input [ type' "range"
                     , value (toString current)
                     , Attr.max (toString max)
                     , on "input" targetValue (send appChannel << ArchiveSelect << toInt' << Debug.watch "range: ")] []
             , text <| toString max]
           , text (toString current)]

toInt' x = case String.toInt x of
             Ok x -> x
             Err str -> Debug.crash str

view : Model -> Html
view {state, current, archive} =
    div []
        [ rangeInput ((Array.length archive)-1) current
        , textbox state.input
        , text state.input
        , div [] (List.map listHtml state.nameList)
        ]

listHtml : String -> Html
listHtml str = li [] [text str]

textbox : String -> Html
textbox str = input [ value str
                    , id "nameInput"
                    , on "input" targetValue (send appChannel << Input << Debug.watch "input: ")
                    , on "keydown" eventObjDecoder (send appChannel << Debug.watch "keydown: ")]
                    []

eventObjDecoder : Json.Decoder Actions
eventObjDecoder =
    Json.customDecoder
        (Json.object2 (,) keyCode targetValue)
        (\(num, str) -> 
            if | num == 8 && String.isEmpty str -> Ok (Delete ())
               | num == 13 && (not <| String.isEmpty str) -> Ok (Add str)
               | otherwise -> Err "")
