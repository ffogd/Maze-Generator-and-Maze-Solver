//Maze.fs
//from Haskell Version http://cdsmith.wordpress.com/2011/06/06/mazes-in-haskell-my-version/
namespace Maze

open System

module MazeType = 
    
    type Cell = int * int

    type Wall = 
        | H of Cell
        | V of Cell

module MazeUtils =
    
    let KnuthShuffle (lst : array<'a>) =
        let Swap i j =                                                
            let item = lst.[i]
            lst.[i] <- lst.[j]
            lst.[j] <- item
        let rnd = new System.Random()
        let ln = lst.Length
        [0..(ln - 2)]                                                  
        |> Seq.iter (fun i -> Swap i (rnd.Next(i, ln)))  
        lst

    let inline getA a (x, y)    = Array2D.get a x y

    let inline updateA a (x, y) = Array2D.set a x y

    let inline addPoint (x,y) (dx,dy) = (x + dx, y + dy)

module UnionFind =
    open MazeUtils

    type UnionFind2D  = 
        {Parents : (int * int)[,]; Ranks : int [,]}    

    let empty = {Parents = Array2D.zeroCreate 0 0; Ranks = Array2D.zeroCreate 0 0}

    let inline root uf i =
        let rec inner fget fupdate i =
            match i = fget i with
            | false -> 
                fupdate i (i|>(fget<<fget))
                inner fget fupdate (fget i)
            | true -> i
        inner (getA uf.Parents) (updateA uf.Parents) i

    let inline find uf (p, q) =
        root uf p = root uf q

    let inline union uf (p, q) =
            let updateParent, updateRank, getRank = 
                updateA uf.Parents<<root uf, updateA uf.Ranks<<root uf, getA uf.Ranks<<root uf
            let unite a b =
                updateParent a b
                updateRank b (getRank b + getRank a)
            match getRank p > getRank q with
            | true  -> unite p q
            | false -> unite q p

module MazeGenerator =
    open MazeType
    open MazeUtils
    open UnionFind

    //processMaze :: UnionFind2D -> Wall list -> Wall list
    let inline processMaze rooms  walls =
        let temp w p q acc =
            match find rooms (p, q) with
            | true -> w :: acc
            | false -> 
                union rooms (p, q)
                acc
        let rec inner w acc =
            match w with
            | [] -> acc
            | H (x,y) :: ws-> inner ws (temp (H(x, y)) (x, y) (x,       y + 1)  acc)
            | V (x,y) :: ws-> inner ws (temp (V(x, y)) (x, y) (x + 1,   y)      acc)
        
        inner walls []
    
    //genMaze :: int -> int -> Wall list
    let inline genMaze w h =
        let parents xmax ymax = Array2D.init xmax ymax (fun x y -> x, y)

        let ranks xmax ymax = Array2D.create xmax ymax 1
        
        let allWalls = 
            Array.append
                [| for x in 0..w-1 do
                   for y in 0..h-2 do
                   yield H(x,y)
                |]
                [| for x in 0..w-2 do
                   for y in 0..h-1 do
                   yield V(x,y)
                |]
        let startRooms = { Parents = parents w h; Ranks = ranks w h}

        KnuthShuffle allWalls 
        |> List.ofArray
        |> processMaze startRooms

module MazeSolver =
    open MazeType
    open MazeUtils
    open Microsoft.FSharp.Collections

    let inline heuristic (x, y) (u, v) = max (abs (x - u))  (abs (y - v))

    // Map<int *int, (int * int) list> -> int-> int -> Point -> Set<Point> 
    let inline successor rooms w h p = 
        let neighbours xs = List.map (addPoint p)  xs
        set[for (u, v) in Map.find p rooms |> neighbours  do
            if (0 <= u && u < w 
                && 0 <= v && v < h) then
                yield u,v
            ]

//    let inline find c =
//          let rec inner x m = 
//              match m with
//              | [] ->  failwith "Can't find tile."
//              | h :: t -> 
//                  match List.tryFindIndex (fun item -> item = c) h with
//                  | Some y -> x, y
//                  | otherwise -> inner (x+1) t
//          inner 0

    let inline run rooms (start, finish) w h solver=
//          let start     = find s maze
//          let finish    = find f maze
          let succ      = successor rooms w h
          
          solver start succ ((=) finish) (fun _ -> 0) (heuristic finish)
//          let cost (x, y) = 0
//              let costs = Map.ofList [('S',1);('F',1);('.',1);('*',2);('^',7)]
//              List.nth m x 
//              |> AstarTypes.flip List.nth y
//              |> AstarTypes.flip Map.find costs        
    
//    let inline input w h = 
//        PSeq.init w (fun x ->PSeq.init h (fun y -> x,y))
