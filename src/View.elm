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
import Html.Attributes exposing (disabled)
import Model exposing (reinit)
import Model exposing (YellowStatus(..))
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
                      
diceLabel color value disabled = 
  let style = [ Background.color (getColor color) , padding 10, spacing 10 ] in
    if disabled then
        Element.el  ((Border.width 2)::(Border.color (rgb255 0 0 0))::style) (Element.text <| String.fromInt value)
    else
        Element.el  style (Element.text <| String.fromInt value)
displayRoll (color,Result(value,status)) = 
     case status of
        Open -> (Input.button [] {onPress = (Just <| Select (color,Result(value,Open))), label = (diceLabel color value False)})
        _ -> diceLabel color value True
     
      
      
    
      

displayGreen (val,mval)=
 let style = [ Background.color (getColor "Green") , padding 10, spacing 10 ] in
  case mval of
     Just val2 -> Element.el style (Element.text ((String.fromInt val) ++ " - " ++ (String.fromInt val2) ++ " = " ++ (String.fromInt (val-val2))))
     Nothing -> Element.el style (Element.text ((String.fromInt val) ++ " - ??"))

displayGray (color,value) =
 let style = [ Background.color (getColor color) , padding 10, spacing 10 ] in
  Element.el style (Element.text(color ++ " - "++ (String.fromInt value)))

displayBlue  value =
  let style = [ Background.color (getColor "Blue") , padding 10, spacing 10 ] in
    Element.el style (Element.text(String.fromInt value))

displayPink  value =
  let style = [ Background.color (getColor "Pink") , padding 10, spacing 10 ] in
    Element.el style (Element.text(String.fromInt value))
displayYellow (value,status) =
    let style = [ Background.color (getColor "Yellow") , padding 10, spacing 10 ] in
        case status of 
            Possible -> Element.el style (Element.text(String.fromInt value))
            Checked -> Element.el ((Border.width 2)::Border.dashed::style) (Element.text(String.fromInt value))
            Done -> Element.el ((Border.width 2)::style) (Element.text(String.fromInt value))

displayPossible (color,value) = 
  let text = diceLabel color value False in
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
         row [height fill, width fill] (List.map displayBlue model.blueBoard),
         row [height fill, width fill] (List.map displayPink model.pinkBoard),
         row [height fill, width fill] (List.map displayYellow model.yellowBoard),
         row [height fill, width fill][(Input.button [] {onPress = Just ReInit, label = Element.text "Reinit"})],

         row [height fill, width fill] [Element.text "Score :",Element.text <| String.fromInt (Model.computeScore(model))]]
        