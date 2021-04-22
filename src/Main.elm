module Main exposing (main)

import Browser
import Html exposing (Html)
import Random
import Dict exposing (..)
import Debug exposing (..)
import Model exposing (DiceRoll(..),init,Model,RollStatus(..),Msg(..), YellowStatus(..))
import View exposing (view)
import Model exposing (YellowStatus)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }




addRoll : Model -> Int -> Model
addRoll model value = 
    case model.rolling of
       color :: q -> { model | dices = Dict.insert color  (Model.Result (value,Model.Open) ) model.dices, rolling = q}
       _ -> model
   
setAside : Int -> Dict String DiceRoll -> Dict String DiceRoll
setAside value dict = 
    Dict.map (\c -> \result -> 
        case result of 
           Model.Result(v,Model.Open) -> if v < value then Result(v,Aside) else Result(v,Open)
           _ -> result) dict

selectGreenDice : Model -> Int -> Model
selectGreenDice model value = 
          case model.greenBoard of 
                (t,Nothing) :: q -> {model | greenBoard = (t,Just value) ::q}
                l -> {model | greenBoard = (value,Nothing)::l}

selectBlueDice : Model -> Int -> Model
selectBlueDice model value = 
    let sum = (Model.getValue model "Blue") + (Model.getValue model "White")
      in {model | blueBoard = sum::model.blueBoard}
  
  
selectPinkDice : Model -> Int -> Model
selectPinkDice model value = {model | pinkBoard = value::model.pinkBoard}

selectYellowDice : Model -> Int -> Model      
selectYellowDice model value = 
   let (l1,l2) = (List.partition (\a -> (Tuple.first a == value && (Tuple.second a) /= Done)) model.yellowBoard) in 
      case l1 of 
        (v,Possible)::q -> {model | yellowBoard = (List.append ((v,Checked)::q) l2)}
        (v,Checked)::q -> {model | yellowBoard = (List.append ((v,Done)::q) l2)} 
        _ -> model

dictToListTuple : (String,DiceRoll) -> (String,Int)
dictToListTuple (color,(Result (val,status))) = (color,val)

    
selectGrayDice : Model -> Int -> String -> Model
selectGrayDice model value pickedColor = 
    let asideDice = Dict.filter (\c -> \result -> 
         case result of 
            Result(v,Open) -> if v < value then True else False
            _ -> False) model.dices
    in let diceList = (pickedColor,value)::List.map  dictToListTuple (Dict.toList asideDice)
    in { model | greyBoard = mergeUniques diceList model.greyBoard}

mergeUniques : List (String,Int) -> List(String,Int) -> List(String,Int)
mergeUniques new old = 
  let insertEl = (\el -> \li -> if List.member el li then li else el::li) in
    List.foldl insertEl new old 


getPossibleSquares : Model -> (String,Int) -> List(String,Int)
getPossibleSquares model (color,value) = 
   if color == "Green" then [("Green",value)] 
        else if color == "Grey" then [("Green",value),("Pink",value),("Yellow",value),("Blue",value)]
        else if color =="Blue" then 
          let head = List.head model.blueBoard in 
            case head of 
              Nothing -> [("Blue",value)]
              Just t -> if (Model.getValue model "Blue") + (Model.getValue model "White") <= t then [("Blue",value)] else []
        else if color == "Pink" then [("Pink",value)]
        else if color == "Yellow" then 
            List.map (\c -> ("Yellow", (Tuple.first c))) (List.filter (\c -> (Tuple.first c) == value && (Tuple.second c) /= Done) model.yellowBoard)
        else []

chooseDice : Model -> (String,DiceRoll) -> Model
chooseDice model (color,Result(value,status)) = 
   let possibleSquares = if color /= "White" then
        getPossibleSquares model (color,value)
                                             else
        List.concatMap (\c -> getPossibleSquares model (c,value)) ["Yellow","Blue","Green","Pink"] 
      in 
        { model | selected = Just(color,value), possibleSquares = possibleSquares }

selectDice : Model -> (String, Int) -> Model
selectDice model (secColor,secValue) = 
  case model.selected of 
      Just (color,value) ->    
          let newDices = Dict.insert color (Result (value,Picked (value))) (setAside value model.dices) in
          let newModel = if color == "Grey" then selectGrayDice model value secColor
                    else if secColor == "Green" then selectGreenDice model value
                    else if secColor == "Blue" then selectBlueDice model value
                    else if secColor == "Pink" then selectPinkDice model value
                    else if secColor == "Yellow" then selectYellowDice model value
                    else model in
             { newModel | dices = newDices, possibleSquares = []  }
      Nothing -> model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReRoll ->
      Debug.log "Roll"
      ( Model.reroll(model),
        Random.generate NewFace (Random.int 1 6))
      
    NewFace newFace ->
       if List.isEmpty model.rolling then
         Debug.log "newFace" ( addRoll model newFace, Cmd.none )
       else 
         Debug.log "newFace" ( addRoll model newFace, Random.generate NewFace (Random.int 1 6) )
    Select (color,diceRoll) -> 
        Debug.log "select" (chooseDice model (color,diceRoll), Cmd.none)
    Pick (color,value) -> 
        (selectDice model (color,value), Cmd.none)
    ReInit -> 
      (Model.reinit(model), Random.generate NewFace (Random.int 1 6) )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



