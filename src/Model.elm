module Model exposing (..)

import Dict exposing (..)

colors = ["Grey","Yellow","Blue","Green","Pink"]

type RollStatus = Open | Picked (Int) | Aside

type DiceRoll = Result (Int, RollStatus)

type alias GreenBoard = List (Int, Maybe Int)





type alias GreyBoard = List (String,Int)

type alias Model =
  { 
      dices : Dict String DiceRoll, 
      rolling : List String,
      selected : Maybe (String,Int),
      possibleSquares : List (String,Int),
      greenBoard : GreenBoard,
      greyBoard : GreyBoard
  }

type Msg = 
    ReRoll
  | NewFace Int
  | Select (String,DiceRoll)
  | Pick (String,Int)

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Dict.empty colors Nothing [] [] []
  , Cmd.none
  )


reroll : Model -> Model
reroll model = {model | rolling = colors}


computeGreenScore : GreenBoard -> Int
computeGreenScore b = 
    computGreenScoreRec b 0

computGreenScoreRec : GreenBoard -> Int -> Int
computGreenScoreRec board score = 
  Debug.log
          (Debug.toString (board))
         (case board of 
                  (val,Just val2)::q -> computGreenScoreRec q (score + (val-val2)) 
                  _ :: q -> computGreenScoreRec q score
                  [] -> score)
             
computeGreyScore : GreyBoard -> Int
computeGreyScore board =  
  let lists = List.map (\color -> (List.filter (\(a,b) -> a == color) board )) colors in   
  let count = (\l -> let len = (List.length l) in case len of 6 -> 22 
                                                              5 -> 16
                                                              4 -> 11
                                                              3 -> 7
                                                              2 -> 4
                                                              1 -> 2
                                                              _ -> 0) in
  List.foldl (\l s -> s + (count l)) 0 lists 

computeScore : Model -> Int 
computeScore model = 
    (computeGreenScore model.greenBoard) + (computeGreyScore model.greyBoard)