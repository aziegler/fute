module Main exposing (main)

import Browser
import Html exposing (Html)
import Random
import Dict exposing (..)
import Debug exposing (..)
import Model exposing (DiceRoll(..),init,Model,RollStatus(..),Msg(..))
import View exposing (view)



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


chooseDice : Model -> (String,DiceRoll) -> Model
chooseDice model (color,Result(value,status)) = 
   let possibleSquares = 
        if color == "Green" then [("Green",value)] 
        else if color == "Grey" then [("Green",value),("Pink",value),("Yellow",value),("Blue",value)] 
        else [] in 
        { model | selected = Just(color,value), possibleSquares = possibleSquares }

selectDice : Model -> (String, Int) -> Model
selectDice model (secColor,secValue) = 
  case model.selected of 
      Just (color,value) ->    
          let newDices = Dict.insert color (Result (value,Picked (value))) (setAside value model.dices) in
          let newModel = if color == "Green" then selectGreenDice model value
                    else if color == "Grey" then selectGrayDice model value secColor
                    else model in
             { newModel | dices = newDices  }
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
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



