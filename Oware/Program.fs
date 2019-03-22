module Oware

open System.Threading.Tasks.Dataflow

type StartingPosition =
    | South
    | North
    
type Board ={
    score:int*int
    CurrentPlayer:StartingPosition
    houses:int*int*int*int*int*int*int*int*int*int*int*int
    }

let playerhouses input= 
    match input with
    | 1| 2 | 3 | 4 | 5 | 6 -> North
    | _ -> South

let playBoard= {houses=(4,4,4,4,4,4,4,4,4,4,4,4);score=(0,0);CurrentPlayer=South}

let getSeeds n board = 
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

let amendedBoard pos board = 
    let {houses=a,b,c,d,e,f,g,h,i,j,k,l}=board
    let {CurrentPlayer=player} = board
    let newBoard=
        match pos with
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
        match player with
        | North -> 0,0,0,0,0,0,g,h,i,j,k,l
        | South -> a,b,c,d,e,f,0,0,0,0,0,0
    match noSeedsToPLay = newBoard with
    | true -> board
    | false -> {board with houses=newBoard}
    
let updateScoreBoard n board player =
    let {score=(p1,p2)} = board
    let {houses=a,b,c,d,e,f,g,h,i,j,k,l}=board
    let {houses=boardHouses}=board
    let totalScore =
        match player with 
        | South -> (p1+n),p2
        | North -> p1,(p2+n)
    let newP = {board with score = totalScore}
    let noSeedsToPLay = 
        match player with
        | North -> 0,0,0,0,0,0,g,h,i,j,k,l
        | South -> a,b,c,d,e,f,0,0,0,0,0,0
    match noSeedsToPLay = boardHouses with
    | true -> newP
    | false -> 
        match player with 
        | South -> {newP with CurrentPlayer=North}
        | North -> {newP with CurrentPlayer=South}
    
let scoreMethod board inputHouseNumber=
    let {CurrentPlayer=player} = board
    let rec innerScoreMethod currboard houseNumber score = 
        match (playerhouses houseNumber) <> player  with
        | true -> updateScoreBoard score currboard player
        | false ->
            match player=(playerhouses houseNumber) with
            | false -> updateScoreBoard score currboard player
            | true ->
                let actualHouseNumber =
                    match houseNumber=0 with
                    | true -> 1
                    |false -> houseNumber
                match getSeeds actualHouseNumber board with
                | 3 -> innerScoreMethod (amendedBoard actualHouseNumber currboard) (actualHouseNumber-1) (score+3) 
                | 2 -> innerScoreMethod (amendedBoard actualHouseNumber currboard) (actualHouseNumber-1) (score+2) 
                | _ -> updateScoreBoard score currboard player
    innerScoreMethod board (inputHouseNumber) 0 

let setSeeds pos board=
    let {CurrentPlayer=player} = board
    let numberOfSeeds = getSeeds pos board
    let {houses=AmendedBoard} = amendedBoard pos board
    match player=( playerhouses pos) with
    | true -> board
    | false ->
        match numberOfSeeds=0 with
        | true -> board
        | false ->
            let rec move housePosition stopCase accBoard =
                let amendhousePosition = 
                    match housePosition<13, stopCase=0 with
                    | false,false -> 1
                    | true,false -> housePosition
                    | _ ->  housePosition-1

                match stopCase=0 with
                | true -> 
                //check who is playing 
                //Are they in the right range
                // Get the values( check 3 back) then update score 
                    let newBoard = {board with houses=accBoard}
                    scoreMethod newBoard amendhousePosition

                | false -> 
                    let a,b,c,d,e,f,g,h,i,j,k,l=accBoard
                    let newAmendedHousePosition =
                        match pos=amendhousePosition with
                        | true -> amendhousePosition+1
                        |false -> amendhousePosition
                    let amendHouse = 
                        match newAmendedHousePosition with
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

    
let useHouse n board = 
    setSeeds n board

let start position = 
    match position with 
    |South -> {playBoard with CurrentPlayer=South}
    |North -> {playBoard with CurrentPlayer=North}

// board -> int*int
let score board = 
    let {score=(finalScore)} = board
    finalScore

// board -> string
let gameState board =
    let {CurrentPlayer=(Playing)} =board
    let {score =(playerOne,playerTwo)}=board
    match (playerOne=24 && playerTwo=24) with
    | true -> "Game ended in a draw"
    | false -> 
        match (playerTwo>=25) with
        | true -> "North won"
        | false -> 
            match (playerOne>=25) with 
            | true -> "South won"
            | false -> 
                match Playing with
                |South -> "South's turn"
                |North -> "North's turn"
    

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
