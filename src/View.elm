module View exposing (..)

import Model exposing (DiceRoll(..),init,Model,RollStatus(..),Msg(..))

import Dict exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (value)
-- VIEW
colorList = Dict.fromList [("Yellow",(rgb255 255 255 0)),
             ("Grey",(rgb255 160 160 160)),
             ("Green",(rgb255 0 200 0)),
             ("Blue",(rgb255 40 40 217)),
             ("Pink",(rgb255 255 100 150)),
             ("White",(rgb255 255 255 255))]


getColor colorName = 
  let potentialColor = (Dict.get colorName colorList) in
  case potentialColor of Just c -> c 
                         Nothing -> rgb 0 0 0
                      
diceLabel color value = 
  let actualColor = (getColor color) in
    Element.el [ Background.color actualColor , padding 10, spacing 10 ] (Element.text <| String.fromInt value)
displayRoll (color,Result(value,status)) = 
    let textElem =  diceLabel color value in  
     case status of
        Open -> (Input.button [] {onPress = (Just <| Select (color,Result(value,Open))), label = textElem})
        _ -> textElem
     
      
      
    
      

displayGreen (val,mval)=
  case mval of
     Just val2 -> Element.text ((String.fromInt val) ++ " - " ++ (String.fromInt val2) ++ " = " ++ (String.fromInt (val-val2)))
     Nothing -> Element.text ((String.fromInt val) ++ " - ??")

displayGray (color,value) =
  Element.text(color ++ " - "++ (String.fromInt value))

displayPossible (color,value) = 
  let text = diceLabel color value in
    (Input.button [] {onPress = (Just <| Pick (color,value)), label = text})



view : Model -> Html Msg
view model =
  Debug.log "View"
  layout []<|
        column [height fill, width fill]
        [row [height fill, width fill]
        ((Input.button [] {onPress = Just ReRoll, label = Element.text "Roll"})::((List.map displayRoll) (Dict.toList model.dices))),
         row [height fill, width fill] (List.map displayPossible model.possibleSquares),
         row [height fill, width fill] (List.map displayGreen model.greenBoard),
         row [height fill, width fill] (List.map displayGray model.greyBoard),
         row [height fill, width fill][],

         row [height fill, width fill] [Element.text "Score :",Element.text <| String.fromInt (Model.computeScore(model))]]
        