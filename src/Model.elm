module Model exposing (..)

import Dict exposing (..)

colors = ["Grey","Yellow","Blue","Green","Pink","White"]

type RollStatus = Open | Picked (Int) | Aside

type DiceRoll = Result (Int, RollStatus)

type alias GreenBoard = List (Int, Maybe Int)

type YellowStatus = Possible | Checked | Done
type alias YellowBoard = List (Int,YellowStatus)



type alias GreyBoard = List (String,Int)

type alias Model =
  { 
      dices : Dict String DiceRoll,
      rolling : List String,
      selected : Maybe (String,Int),
      possibleSquares : List (String,Int),
      greenBoard : GreenBoard,
      greyBoard : GreyBoard,
      blueBoard : List(Int),
      pinkBoard : List(Int), 
      yellowBoard : YellowBoard
  }

type Msg = 
    ReRoll
  | ReInit 
  | NewFace Int
  | Select (String,DiceRoll)
  | Pick (String,Int)

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Dict.empty colors Nothing [] [] [] [] [] [(1,Possible),(2,Possible),(2,Possible),(3,Possible),(3,Possible),(4,Possible),(4,Possible),(5,Possible),(5,Possible),(6,Possible)]
  , Cmd.none
  )

reinit : Model -> Model
reinit model = 
    {model | dices = Dict.empty}

getValue : Model -> String -> Int
getValue model color = 
  let diceRoll = Dict.get color model.dices in 
    case diceRoll of
      Just (Result (v,_))-> v
      _ -> 0

reroll : Model -> Model
reroll model = 
    let picked = Dict.filter (\c (Result(i,s)) -> case s of 
                                                        Picked _ -> True  
                                                        _ -> False) model.dices in 
        if (Dict.size model.dices == 0 || Dict.size picked == 3) then 
           {model | rolling = colors} else 
           let open = Dict.filter (\c (Result(i,s)) -> case s of 
                                                        Open -> True  
                                                        _ -> False) model.dices in
                {model | rolling = (List.map Tuple.first (Dict.toList open))}


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

computeBlueScore : List(Int) -> Int
computeBlueScore board = 
    case List.length board of
       0 -> 0
       1 -> 3
       2 -> 6
       3 -> 10
       4 -> 15
       5 -> 21 
       6 -> 28
       7 -> 36
       8 -> 45
       9 -> 55
       10 -> 66
       11 -> 78
       _ -> 78

computePinkScore : List(Int) -> Int
computePinkScore pinkBoard = List.sum pinkBoard

computeYellowScore yellowBoard = 
    let values = List.filter (\c -> Tuple.second c == Done) yellowBoard in
        case List.length values of
            0 -> 0
            1 -> 3
            2 -> 10
            3 -> 21
            4 -> 36
            5 -> 55 
            6 -> 75
            7 -> 96
            8 -> 118
            9 -> 141
            10 -> 165
            _ -> 165
computeScore : Model -> Int 
computeScore model = 
    (computeGreenScore model.greenBoard) + (computeGreyScore model.greyBoard) + (computeBlueScore model.blueBoard) + (computePinkScore model.pinkBoard) + (computeYellowScore model.yellowBoard)