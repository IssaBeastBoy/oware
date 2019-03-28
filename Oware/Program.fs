﻿module Oware

open System.Threading.Tasks.Dataflow
open System.Drawing
//Most of the work was done on one computer with all three of us giving input and ideas, these comments are to show my understanding of the work we all did together 
type StartingPosition = 
    | South
    | North
    
type Board ={ //The board we use to play on
    score:int*int
    CurrentPlayer:StartingPosition
    houses:int*int*int*int*int*int*int*int*int*int*int*int
    }
//int->StartingPostion
let playerhouses input= 
    match input with
    | 1| 2 | 3 | 4 | 5 | 6 -> North
    | _ -> South

let playBoard= {houses=(4,4,4,4,4,4,4,4,4,4,4,4);score=(0,0);CurrentPlayer=South} //Creating the board at the start of the game with South's turn first

//int->board->int
let getSeeds n board = //This function extracts the desired seeds from the board 
    let {houses=a,b,c,d,e,f,g,h,i,j,k,l}=board
    match n with
    | 1 -> a
    | 2 -> b
    | 3 -> c
    | 4 -> d
    | 5 -> e
    | 6 -> f
    | 7 -> g 
    | 8 -> h
    | 9 -> i
    | 10 -> j
    | 11 -> k
    | 12 -> l

 //board->StartingPostion->bool
let CheckHousesIfAllZero board player =
    let housetoStart = 
        match player with 
        | South -> 1
        | North -> 7
    
    let rec checkHouses housesWehave counter acc =
        match counter = 0 with 
        | true -> acc
        | false -> 
            match (getSeeds housesWehave board) with
            | 0 -> checkHouses (housesWehave+1) (counter-1) true
            | _ -> false
    checkHouses housetoStart 6 false

//int->board->board
let amendedBoard pos board = 
    let {houses=a,b,c,d,e,f,g,h,i,j,k,l}=board
    let {CurrentPlayer=player} = board
    let newBoard=
        match pos with //When the turn starts this will set the house that was selected to 0
        | 1 -> 0,b,c,d,e,f,g,h,i,j,k,l
        | 2 -> a,0,c,d,e,f,g,h,i,j,k,l
        | 3 -> a,b,0,d,e,f,g,h,i,j,k,l
        | 4 -> a,b,c,0,e,f,g,h,i,j,k,l
        | 5 -> a,b,c,d,0,f,g,h,i,j,k,l
        | 6 -> a,b,c,d,e,0,g,h,i,j,k,l
        | 7 -> a,b,c,d,e,f,0,h,i,j,k,l 
        | 8 -> a,b,c,d,e,f,g,0,i,j,k,l
        | 9 -> a,b,c,d,e,f,g,h,0,j,k,l
        | 10 -> a,b,c,d,e,f,g,h,i,0,k,l
        | 11 -> a,b,c,d,e,f,g,h,i,j,0,l
        | 12 -> a,b,c,d,e,f,g,h,i,j,k,0
    let noSeedsToPLay = 
        match player with //This checks if the current player can play on their side
        | North -> 0,0,0,0,0,0,g,h,i,j,k,l
        | South -> a,b,c,d,e,f,0,0,0,0,0,0
    match (noSeedsToPLay = newBoard) , (CheckHousesIfAllZero board player) with //If all of them are 0 with should return a new board that's able to be played
    | true,false -> board
    | true,true | _ -> {board with houses=newBoard}

//board->int*int*int*int*int*int*int*int*int*int*int*int
let noSeedsToPLay board= 
    let {houses=a,b,c,d,e,f,g,h,i,j,k,l}=board
    let {CurrentPlayer=player} = board
    match player with
    | North -> 0,0,0,0,0,0,g,h,i,j,k,l
    | South -> a,b,c,d,e,f,0,0,0,0,0,0
    
//int->board->StartingPostion->board
let updateScoreBoard n board player =
    let {score=(p1,p2)} = board
    let {houses=boardHouses}=board
    let totalScore = //When the seeds are captured this will increment the score for the player on the turn the seeds were captured
        match player with  
        | South -> (p1+n),p2
        | North -> p1,(p2+n)
    let newP = {board with score = totalScore}
    match (noSeedsToPLay board) = boardHouses with
    | true -> newP
    | false -> 
        match player with 
        | South -> {newP with CurrentPlayer=North}
        | North -> {newP with CurrentPlayer=South}
 
//board->int->board 
let scoreMethod board inputHouseNumber=
    let {CurrentPlayer=player} = board
    let rec innerScoreMethod currboard houseNumber score = //This function is for scoring the player
        match (playerhouses houseNumber) <> player  with
        | true -> updateScoreBoard score currboard player //Upadting the score for current player
        | false ->
            match player=(playerhouses houseNumber) with
            | false -> updateScoreBoard score currboard player
            | true ->
                let actualHouseNumber =
                    match houseNumber=0 with
                    | true -> 1
                    | false -> houseNumber

                let newBoard = amendedBoard actualHouseNumber currboard
                
                match newBoard=currboard with
                | true -> updateScoreBoard score currboard player
                | false -> 
                    match getSeeds actualHouseNumber board with //If there are more seeds to capture move back and capture them until it can't be capture anymore
                    | 3 -> innerScoreMethod newBoard (actualHouseNumber-1) (score+3) 
                    | 2 -> innerScoreMethod newBoard (actualHouseNumber-1) (score+2) 
                    | _ -> updateScoreBoard score currboard player
    innerScoreMethod board (inputHouseNumber) 0 

//int->board->board
let setSeeds pos board=
    let {CurrentPlayer=player} = board
    let numberOfSeeds = getSeeds pos board
    let {houses=AmendedBoard} = amendedBoard pos board
    match player=( playerhouses pos) with
    | true -> board
    | false ->
        match numberOfSeeds=0 with //If there are no seeds to sow, return the board as is
        | true -> board
        | false ->
            let rec move housePosition stopCase accBoard = //This function is to sow the seeds while going around the board
                let amendhousePosition = 
                    match housePosition<13, stopCase=0 with //House position will loop back to one if it is 13
                    | false,false -> 1
                    | true,false -> housePosition
                    | _ ->  housePosition-1

                match stopCase=0 with
                | true -> 
                    let newBoard = {board with houses=accBoard}
                    match (noSeedsToPLay newBoard) = accBoard with
                    | true -> board
                    | false ->
                        scoreMethod newBoard amendhousePosition

                | false -> 
                    let a,b,c,d,e,f,g,h,i,j,k,l=accBoard
                    let newAmendedHousePosition =
                        match pos=amendhousePosition with
                        | true -> amendhousePosition+1
                        |false -> amendhousePosition
                    let amendHouse = 
                        match newAmendedHousePosition with //Matches position with house and increments them from the tuple
                        | 1 -> a+1,b,c,d,e,f,g,h,i,j,k,l
                        | 2 -> a,b+1,c,d,e,f,g,h,i,j,k,l
                        | 3 -> a,b,c+1,d,e,f,g,h,i,j,k,l
                        | 4 -> a,b,c,d+1,e,f,g,h,i,j,k,l
                        | 5 -> a,b,c,d,e+1,f,g,h,i,j,k,l
                        | 5 -> a,b,c,d,e+1,f,g,h,i,j,k,l
                        | 6 -> a,b,c,d,e,f+1,g,h,i,j,k,l
                        | 7 -> a,b,c,d,e,f,g+1,h,i,j,k,l 
                        | 8 -> a,b,c,d,e,f,g,h+1,i,j,k,l
                        | 9 -> a,b,c,d,e,f,g,h,i+1,j,k,l
                        | 10 -> a,b,c,d,e,f,g,h,i,j+1,k,l
                        | 11 -> a,b,c,d,e,f,g,h,i,j,k+1,l
                        | 12 -> a,b,c,d,e,f,g,h,i,j,k,l+1
                    move (newAmendedHousePosition+1) (stopCase-1) amendHouse
            move (pos+1) numberOfSeeds AmendedBoard

    
//int->board->board
let useHouse n board = 
    setSeeds n board

//StartingPosition->board
let start position =  //Whereever you choose to start this will return the board with that player
    match position with 
    | South -> {playBoard with CurrentPlayer=South} 
    | North -> {playBoard with CurrentPlayer=North}

// board -> int*int
let score board = 
    let {score=(finalScore)} = board
    finalScore

// board -> string
let gameState board =
    let {CurrentPlayer=(Playing)} =board
    let {score =(playerOne,playerTwo)}=board
    match (playerOne=24 && playerTwo=24) with //If each player has 24 seeds the game ends in a draw
    | true -> "Game ended in a draw"
    | false -> 
        match (playerTwo>=25) with //Once the player has 25 seeds or more 
        | true -> "North won"
        | false -> 
            match (playerOne>=25) with 
            | true -> "South won"
            | false -> 
                match Playing with //If not, keep playing
                |South -> "South's turn"
                |North -> "North's turn"
    

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
