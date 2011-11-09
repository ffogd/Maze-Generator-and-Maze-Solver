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

    let inline run rooms (start, finish) w h solver=
          let succ      = successor rooms w h
          
          solver start succ ((=) finish) (fun _ -> 0) (heuristic finish)

//module JumpPointType =
//  open MazeType
//
//  type StraightDirection = N  | S | E  | W  
//  type DiagonalDirection = NE | NW | SE | SW 
//  type Direction =
//    | Straight of StraightDirection * Cell
//    | Diagonal of DiagonalDirection * Cell
//    | NONE
//
//
//  type JumpPointEnvironment = 
//        { rooms : Map<int *int, Direction list>; 
//            w : int; h : int;}
//  
//  let empty = { rooms = Map.empty; w = 20; h = 20;}
//  type NotRule = Not of Direction
//
//  let inline straightPosition direction =
//    match direction with
//    | N -> (0, -1)
//    | S -> (0, 1)
//    | E -> (1, 0)
//    | W -> (-1, 0)
//  
//  let diagonalPosition direction =
//    match direction with
//    | NE -> (1, -1)
//    | SE -> (1, 1)
//    | SW -> (-1, 1)
//    | NW -> (-1, -1)
//  
//  let inline straight  direction = Straight (direction, straightPosition direction)
//  let inline diagonal  direction = Diagonal (direction, diagonalPosition direction)
//
//
//  let inline notRule direction = Not (straight direction)
//  
//  type Rule = 
//      | RuleForStraight of (NotRule * Direction) 
//      | RuleForDiagonal of Direction
//  
//  let straightRule notrule direction  = RuleForStraight (notrule, diagonal direction) 
//  let diagonalRule direction  = RuleForDiagonal (straight direction)
//
//  let inline flip f a b = f b a
//  let inGrid w h cell =
//            match cell with
//            | x, y when (0 <= x && x <= w - 1 && 0 <= y && y <= h - 1) -> true
//            | _  -> false
//
//module JumpPointSearch  =
//  open Microsoft.FSharp.Collections
//  open Astar
//  open JumpPointType
//  
//  let inline successor rooms p = 
//      set[for d in Map.find p rooms  do
//              yield d
//          ]
//  let sqrtTWO = 1.414213562
//
//  let inline heuristic (x1, y1) (x2, y2) =
//      max (abs (x1 - x2))  (abs (y1 - y2))|>float
//
//  let rec jump isGoal mazeEnv x y jumpDirection =
//          let directionSteps dx dy notObstacle =
//                (x, y)
//                |> Seq.unfold (fun cell -> 
//                                    let nextCell = MazeUtils.addPoint cell (dx, dy)
//                                    if inGrid mazeEnv.w mazeEnv.h nextCell && notObstacle cell then 
//                                        Some(nextCell, nextCell) 
//                                    else None) 
//          let directionMovies direction = 
//                match direction with
//                | NONE -> Seq.empty
//                | Straight(_,(dx, dy)) -> directionSteps dx dy (Set.contains direction << successor mazeEnv.rooms)
//                | Diagonal(_,(dx, dy)) -> directionSteps dx dy (Set.contains direction << successor mazeEnv.rooms)
//
//          let move direction directionRules =
//              let applayRules rules func (px, py) = 
//                    Seq.map (fun rule -> 
//                                    match rule with
//                                    | RuleForStraight(Not a, b) -> (not <| func a) && func b 
//                                    | RuleForDiagonal d -> jump isGoal mazeEnv px py d |> Option.isSome) rules |> Seq.reduce (||)
//              let movies = directionMovies direction  
//              movies
//              |> Seq.tryFind (fun p ->
//                    isGoal p || applayRules directionRules (flip Set.contains (successor mazeEnv.rooms p)) p)
//
//              
//          match jumpDirection with
//          | NONE -> None
//          | Straight(N, _) as dir -> move dir [straightRule (notRule W) NW; straightRule (notRule E) NE]
//
//          | Straight(S, _) as dir -> move dir [straightRule (notRule W) SW; straightRule (notRule E) SE] 
//
//          | Straight(E, _) as dir -> move dir [straightRule (notRule S) SE; straightRule (notRule N) NE]
//
//          | Straight(W, _) as dir -> move dir [straightRule (notRule S) SW; straightRule (notRule N) NW]
//
//          | Diagonal(NE, _) as dir -> move dir [straightRule (notRule S) SE; straightRule (notRule W) NW;
//                                                diagonalRule N; diagonalRule E] 
//
//          | Diagonal(SE, _) as dir -> move dir [straightRule (notRule W) SW; straightRule (notRule N) NE;
//                                                diagonalRule S; diagonalRule E]
//          | Diagonal(SW, _) as dir -> move dir [straightRule (notRule N) NW; straightRule (notRule E) SE;
//                                                diagonalRule S; diagonalRule W]
//          | Diagonal(NW, _) as dir -> move dir [straightRule (notRule E) NE; straightRule (notRule S) SW;
//                                                diagonalRule N; diagonalRule W]
//
//  let inline directionsTopoints directions (x, y)=
//      let ns d = 
//              match d with
//              | Straight(_, (dx, dy)) ->    x + dx, y + dy
//              | Diagonal(_, (dx, dy)) ->    x + dx, y + dy
//              | NONE -> failwith "failed to determine direction."
//      Set.map ns directions
//
//  let inline findJumpPoints (x, y) d isGoal neighbours mazeEnv =
//      let find ds neighbour =
//          ds @ 
//              (neighbour
//               |> List.filter (not << flip Set.contains neighbours << fst)
//               |> List.map snd)
//          |> List.choose (jump isGoal  mazeEnv x y)
//      match d with
//      | Straight(N, _) -> 
//          find [straight S]
//                    (List.zip   [straight E;     straight W] 
//                                [diagonal SE;    diagonal SW])
//
//      | Diagonal(NE, _) -> 
//          find [diagonal SW; straight S; straight W]
//                    (List.zip   [straight N;     straight E] 
//                                [diagonal NW;    diagonal SE])
//
//      | Straight(E, _) ->
//          find [straight W]
//                    (List.zip   [straight N;     straight S] 
//                                [diagonal NW;    diagonal SW])
//      | Straight(S, _) -> 
//          find [straight N] 
//                    (List.zip   [straight E;     straight W] 
//                                [diagonal NE;    diagonal NW])
//      | Diagonal(SE, _) -> 
//          find [diagonal NW; straight N; straight W]   
//                    (List.zip   [straight S;     straight E] 
//                                [diagonal SW;    diagonal NE])
//      | Straight(W, _) -> 
//         find [straight E]           
//                    (List.zip   [straight N;     straight S] 
//                                [diagonal NE;    diagonal SE])
//      | Diagonal(SW, _) -> 
//          find [diagonal NE; straight N; straight E]   
//                    (List.zip   [straight S;     straight W] 
//                                [diagonal SE;    diagonal NW])
//      | Diagonal(NW, _) -> 
//          find [diagonal SE; straight S; straight E]
//                   (List.zip   [straight N;     straight W] 
//                               [diagonal NE;    diagonal SW])
//      | NONE -> directionsTopoints neighbours (x, y) |> Set.toList
//         
//
//  let inline directionToParent  parent target =
//      match parent, target with
//      | (px, py), (x, y) when py = y && px = x -> NONE
//      | (px, py), (x, y) when py = y && px > x -> straight E
//      | (px, py), (x, y) when py = y && px <= x -> straight W
//      | (px, py), (x, y) when py < y && px < x -> diagonal NW
//      | (px, py), (x, y) when py < y && px > x -> diagonal NE
//      | (px, py), (x, y) when py < y && px = x -> straight N
//      | (px, py), (x, y) when py > y && px < x -> diagonal SW
//      | (px, py), (x, y) when py > y && px > x -> diagonal SE
//      | (px, py), (x, y) when py > y && px = x -> straight S
//      | _ -> failwith "failed to determine direction to parent!"   
//
//  
//  let astarJump start mazeEnv finish cost heur = 
//      let dcosts (x1,y1) (x2,y2) = 
//        let mn = (min (abs(x1-x2)) (abs(y1-y2)))
//        let mx = (max (abs(x1-x2)) (abs(y1-y2)))
//        (sqrtTWO - 1.0) * (float mn) + (float mx) 
//      let inner (seen, q)  =
//           match PriorityQueue.isEmpty q with
//           | true -> failwith "No Solution."
//           | false ->
//               let ((c, next), dq) = PriorityQueue.deleteFindMin q
//               let curr, parent = next
//               if c = 0.0 then None
//               else 
//                   match finish curr with
//                   | true -> Some ((curr, (parent, c)),(seen, PriorityQueue.singleton 0.0 (curr, curr)))
//                   | otherwise -> 
//                       let succs = successor mazeEnv.rooms curr //succ n
//                       let dir = directionToParent parent curr
//                       let tests = findJumpPoints curr dir finish succs mazeEnv |> Set.ofSeq
// 
//                       let costs target = c + (dcosts curr target) + (heur target) - (heur curr) 
//                       let makePair = tests |> Set.map (fun cell -> cell, curr)
//                       let q' = 
//                           Set.difference makePair seen |> Seq.map (fun (a, b) -> costs a, (a,b)) //Set.difference tests seen
//                           |> PriorityQueue.ofSeq |> PriorityQueue.merge dq
//                       Some ((curr, (parent, c)), ((Set.union seen makePair), q'))
//                   
//      Seq.unfold inner ((Set.singleton (start,start)), (PriorityQueue.singleton (heur start) (start,start))) 
//
//  let astarJump1 start mazeEnv finish cost heur = 
//      let stepCosts (x1,y1) (x2,y2) = 
//        let mn = (min (abs(x1-x2)) (abs(y1-y2)))
//        let mx = (max (abs(x1-x2)) (abs(y1-y2)))
//        (sqrtTWO - 1.0) * (float mn) + (float mx) 
//      let getCurrentAndParent l =
//        match l with
//        | target :: parent :: _-> target, parent
//        | [target] -> target, target
//        | [] -> failwith "failed to determine direction to parent!" 
//      let rec inner seen q  =
//           match PriorityQueue.isEmpty q with
//           | true -> failwith "No Solution."
//           | false ->
//               let ((c, next), dq) = PriorityQueue.deleteFindMin q
//               let curr, parent = getCurrentAndParent next
// 
//               match finish curr with
//               | true -> next
//               | otherwise -> 
//                   let succs = successor mazeEnv.rooms curr 
//                   let dir = directionToParent parent curr
//                   let tests = findJumpPoints curr dir finish succs mazeEnv |> Set.ofList
// 
//                   let costs target = c + (stepCosts curr target) + (heur target) - (heur curr) 
////                   let makePair = tests|>Set.map (fun cell -> curr,cell)
//
//                   let q' = 
//                       
//                       Set.difference tests seen |> Seq.map (fun x -> costs x, x :: next) 
//                       |> PriorityQueue.ofSeq |> PriorityQueue.merge dq
//                   inner (Set.union seen tests) q' 
//                                    
//      inner (Set.singleton (start)) (PriorityQueue.singleton (heur start) [start]) 
  
