module Oware

type StartingPosition =
    | South
    | North



type Board ={
    houses:int*int*int*int*int*int*int*int*int*int*int*int
    }

type Player=
    |Player1 of bool
    |Player2 of bool

let p1Score=0
let p2Score=0

let playBoard= {houses=(4,4,4,4,4,4,4,4,4,4,4,4)}

let getSeeds n board = 
    let a,b,c,d,e,f,g,h,i,j,k,l=board
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

let setSeeds pos board=
    let houseNum = getSeeds pos board
    let a,b,c,d,e,f,g,h,i,j,k,l=board
    let AmendedBoard = 
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

    let rec move housePosition stopCase accBoard =
        let amendhousePosition = 
            match housePosition with
            | 12 -> 1
            | _ -> housePosition

        match stopCase=0 with
        | true -> accBoard
        | false -> 
            let a,b,c,d,e,f,g,h,i,j,k,l=accBoard
            let amendHouse = 
                match housePosition with
                | 1 -> a+1,b,c,d,e,f,g,h,i,j,k,l
                | 2 -> a,b+1,c,d,e,f,g,h,i,j,k,l
                | 3 -> a,b,c+1,d,e,f,g,h,i,j,k,l
                | 4 -> a,b,c,d+1,e,f,g,h,i,j,k,l
                | 5 -> a,b,c,d,e+1,f,g,h,i,j,k,l
                | 6 -> a,b,c,d,e,f+1,g,h,i,j,k,l
                | 7 -> a,b,c,d,e,f,g+1,h,i,j,k,l 
                | 8 -> a,b,c,d,e,f,g,h+1,i,j,k,l
                | 9 -> a,b,c,d,e,f,g,h,i+1,j,k,l
                | 10 -> a,b,c,d,e,f,g,h,i,j+1,k,l
                | 11 -> a,b,c,d,e,f,g,h,i,j,k+1,l
                | 12 -> a,b,c,d,e,f,g,h,i,j,k,l+1
            move (amendhousePosition+1) (stopCase-1) amendHouse

    move (pos+1) houseNum AmendedBoard

    
let useHouse n board = 
    setSeeds n board 

let start position = 
    match position with 
    |South -> playBoard.houses
    |North ->  playBoard.houses

let score board = failwith "4 "

let gameState board = failwith "5"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
