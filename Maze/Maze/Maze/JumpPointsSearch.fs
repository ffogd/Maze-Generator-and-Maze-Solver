namespace Maze

open System
// from http://harablog.wordpress.com/2011/09/07/jump-point-search/

module JumpPointSearchType =
  open MazeType

  type StraightDirection = N  | S | E  | W  
  type DiagonalDirection = NE | NW | SE | SW
   
  type Direction =
    | Straight of StraightDirection * Cell
    | Diagonal of DiagonalDirection * Cell
    | NONE

  type JumpPointEnvironment = 
    { grid : Map<int * int, Direction list>;  //cells with avialiable Directions. 
      w : int; h : int;     // Weight and Height
      isGoal : int * int -> bool;
      heuristic : int * int -> float;
      stepCosts : int * int -> int * int -> float}
        member x.successors point = 
          set[for direction in Map.find point x.grid  do
                  yield direction
              ]

  let empty = { grid = Map.empty; w = 20; h = 20; isGoal = (fun _ -> true);
                heuristic = (fun _ -> 0.0)
                stepCosts = (fun _ _-> 0.0)
               }

  let inline straightPosition direction =
    match direction with
    | N -> (0, -1)
    | S -> (0, 1)
    | E -> (1, 0)
    | W -> (-1, 0)
  
  let diagonalPosition direction =
    match direction with
    | NE -> (1,  -1)
    | SE -> (1,   1)
    | SW -> (-1,  1)
    | NW -> (-1, -1)
  
  let inline straight  direction = Straight (direction, straightPosition direction)
  let inline diagonal  direction = Diagonal (direction, diagonalPosition direction)

  type NotRule = Not of Direction

  let inline notRule direction = Not (straight direction)
  
  type PruningRule = 
      | StraightRule of (NotRule * Direction) 
      | DiagonalRule of Direction
  
  let inline straightRule notrule direction  = StraightRule (notrule, diagonal direction) 
  let inline diagonalRule direction  = DiagonalRule (straight direction)

  let inline flip f a b = f b a

  let inline inGrid w h cell =
            match cell with
            | x, y when (0 <= x && x <= w - 1 && 0 <= y && y <= h - 1) -> true
            | _  -> false

module JumpPointsSearch =
  open Microsoft.FSharp.Collections
  open Astar
  open JumpPointSearchType
  
  let sqrtTWO = 1.414213562
  
  let inline diagHeuristic (x1, y1) (x2, y2) =
    let diagonal = min (abs(x1 - x2)) (abs (y1 - y2)) |> float
    let straight = (abs (x1 - x2)) + (abs (y1 - y2)) |> float
    sqrtTWO * diagonal + (straight - 2.0 * diagonal)


  let inline stepCosts (x1,y1) (x2,y2) = 
        let xa, ya = abs(x1-x2), (abs(y1-y2))
        (sqrtTWO - 1.0) * (min xa ya |> float) + (max xa ya |> float) 

  // move with turning points rules in direction jumpDirection.
  // recursively apply the straight pruning rule or
  // the diagonal pruning rule.
  // jump : JumpPointEnvironment -> int -> int -> Direction -> (int * int) option
  let rec jump env x y jumpDirection =
          let generateSteps dx dy notObstacle =
                (x, y)
                |> Seq.unfold (fun cell -> 
                                    let nextCell = MazeUtils.addPoint cell (dx, dy)
                                    if inGrid env.w env.h nextCell && notObstacle cell then 
                                        Some(nextCell, nextCell) 
                                    else None) 
          let directionSteps direction = 
                match direction with
                | NONE -> Seq.empty
                | Straight(_,(dx, dy)) -> generateSteps dx dy (Set.contains direction << env.successors )
                | Diagonal(_,(dx, dy)) -> generateSteps dx dy (Set.contains direction << env.successors )
          
          
          let move direction directionRules =
              //apply the pruning rules.
              let applayRules rules func (px, py) = 
                    Seq.map (fun rule -> 
                                    match rule with
                                    | StraightRule(Not a, b) -> (not <| func a) && func b 
                                    | DiagonalRule dir -> jump env px py dir |> Option.isSome) rules |> Seq.reduce (||)
              //all available steps in current direction.
              let steps = directionSteps direction 
              //try to find jump point p. 
              steps
              |> Seq.tryFind (fun p ->
                    env.isGoal p || applayRules directionRules (flip Set.contains (env.successors p)) p)
                        
          match jumpDirection with
          | NONE -> None
          
          | Straight(N, _) as dir -> 
            //(x, y) is a jump point if a NW neighbour exists which cannot be                                  
            // reached by a shorter path than one involving (x, y) or with other words W is obstacle or
            // if NE and not E
                                     move dir [straightRule (notRule W) NW; straightRule (notRule E) NE]

          | Straight(S, _) as dir -> move dir [straightRule (notRule W) SW; straightRule (notRule E) SE] 

          | Straight(E, _) as dir -> move dir [straightRule (notRule S) SE; straightRule (notRule N) NE]

          | Straight(W, _) as dir -> move dir [straightRule (notRule S) SW; straightRule (notRule N) NW]
          
          | Diagonal(NE, _) as dir -> 
            //(x, y) is a jump point if a SE neighbour exists which cannot be                                  
            // reached by a shorter path than one involving (x, y) or with other words S is obstacle or
            // if NW and not W or 
            // if we can reach other jump points by 
            // travelling vertically or horizontally.  
                                      move dir [straightRule (notRule S) SE; straightRule (notRule W) NW;
                                                diagonalRule N; diagonalRule E] 

          | Diagonal(SE, _) as dir -> move dir [straightRule (notRule W) SW; straightRule (notRule N) NE;
                                                diagonalRule S; diagonalRule E]
          | Diagonal(SW, _) as dir -> move dir [straightRule (notRule N) NW; straightRule (notRule E) SE;
                                                diagonalRule S; diagonalRule W]
          | Diagonal(NW, _) as dir -> move dir [straightRule (notRule E) NE; straightRule (notRule S) SW;
                                                diagonalRule N; diagonalRule W]
  
  // directionsToPoints : Set<Direction> -> int * int -> Set<int * int>
  let inline directionsToPoints directions (x, y)=
      let inner d = 
              match d with
              | Straight(_, (dx, dy)) ->    x + dx, y + dy
              | Diagonal(_, (dx, dy)) ->    x + dx, y + dy
              | NONE -> failwith "failed to determine direction."
      Set.map inner directions

  //  findJumpPoints : JumpPointEnvironment -> int * int -> Direction -> Set<Direction> -> (int * int) list
  let inline findJumpPoints env (x, y) direction neighbours  =
      let find naturalNeighbours forcedNeighboursRules =
          naturalNeighbours @ 
              (forcedNeighboursRules
               |> List.filter (not << flip Set.contains neighbours << fst)
               |> List.map snd)
          |> List.choose (jump env x y)
      //Neighbour Pruning Rules
      match direction with
      | Straight(N, _) -> 
          // add S neighbour to the pruned set of neighbours.
          // add SE neighbour only if E neighbour is obstacle.
          // add SW neighbour only if W neighbour is obstacle.
          find [straight S]
                    (List.zip   [straight E;     straight W] 
                                [diagonal SE;    diagonal SW])

      | Diagonal(NE, _) -> 
          find [diagonal SW; straight S; straight W]
                    (List.zip   [straight N;     straight E] 
                                [diagonal NW;    diagonal SE])

      | Straight(E, _) ->
          find [straight W]
                    (List.zip   [straight N;     straight S] 
                                [diagonal NW;    diagonal SW])
      | Straight(S, _) -> 
          find [straight N] 
                    (List.zip   [straight E;     straight W] 
                                [diagonal NE;    diagonal NW])
      | Diagonal(SE, _) -> 
          find [diagonal NW; straight N; straight W]   
                    (List.zip   [straight S;     straight E] 
                                [diagonal SW;    diagonal NE])
      | Straight(W, _) -> 
         find [straight E]           
                    (List.zip   [straight N;     straight S] 
                                [diagonal NE;    diagonal SE])
      | Diagonal(SW, _) -> 
          find [diagonal NE; straight N; straight E]   
                    (List.zip   [straight S;     straight W] 
                                [diagonal SE;    diagonal NW])
      | Diagonal(NW, _) -> 
          find [diagonal SE; straight S; straight E]
                   (List.zip   [straight N;     straight W] 
                               [diagonal NE;    diagonal SW])
      // return all neighbours to the pruned set of neighbours.
      | NONE -> directionsToPoints neighbours (x, y) |> Set.toList
         

  let inline directionToParent  parent target =
      match parent, target with
      | (px, py), (x, y) when py = y && px = x -> NONE
      | (px, py), (x, y) when py = y && px > x -> straight E
      | (px, py), (x, y) when py = y && px <= x -> straight W
      | (px, py), (x, y) when py < y && px < x -> diagonal NW
      | (px, py), (x, y) when py < y && px > x -> diagonal NE
      | (px, py), (x, y) when py < y && px = x -> straight N
      | (px, py), (x, y) when py > y && px < x -> diagonal SW
      | (px, py), (x, y) when py > y && px > x -> diagonal SE
      | (px, py), (x, y) when py > y && px = x -> straight S
      | _ -> failwith "failed to determine direction to parent!"   

  // return all jump points with parents and costs.  seq<jumpPoint   * (parent      * cost)> 
  //astarJump : int * int -> JumpPointEnvironment -> seq<(int * int) * ((int * int) * float)>
  let inline astarJump start env = 
      let inner (seen, q)  =
           match PriorityQueue.isEmpty q with
           | true -> failwith "No Solution."
           | false ->
               let ((currentCosts, next), dq) = PriorityQueue.deleteFindMin q
               let expanded, parent = next
               if currentCosts = 0.0 then None
               else 
                   match env.isGoal expanded with
                   | true -> Some ((expanded, (parent, currentCosts)),(seen, PriorityQueue.singleton 0.0 (expanded, expanded)))
                   | otherwise -> 
                       let succs = env.successors expanded
                       let dir = directionToParent parent expanded
                       let tests = findJumpPoints env expanded dir succs  |> Set.ofSeq
 
                       let costs target = currentCosts + (env.stepCosts expanded target)  
                                            + (env.heuristic target) - (env.heuristic expanded) 

                       let q' = 
                           Set.difference tests seen |> Seq.map (fun a -> costs a, (a, expanded)) 
                           |> PriorityQueue.ofSeq |> PriorityQueue.merge dq
                       Some ((expanded, (parent, currentCosts)), ((Set.union seen tests), q'))
                   
      Seq.unfold inner ((Set.singleton start), (PriorityQueue.singleton (env.heuristic start) (start,start))) 
  
  let astarJump1 start mazeEnv = 
      let getCurrentAndParent l =
        match l with
        | target :: parent :: _-> target, parent
        | [target] -> target, target
        | [] -> failwith "failed to determine direction to parent!" 
      let rec inner seen q  =
           match PriorityQueue.isEmpty q with
           | true -> failwith "No Solution."
           | false ->
               let ((c, next), dq) = PriorityQueue.deleteFindMin q
               let curr, parent = getCurrentAndParent next
 
               match mazeEnv.isGoal curr with
               | true -> next
               | otherwise -> 
                   let succs = mazeEnv.successors curr 
                   let dir = directionToParent parent curr
                   let tests = findJumpPoints mazeEnv curr dir succs  |> Set.ofList
 
                   let costs target = c + (mazeEnv.stepCosts curr target) + (mazeEnv.heuristic target) - (mazeEnv.heuristic curr)  

                   let q' = 
                       
                       Set.difference tests seen |> Seq.map (fun x -> costs x, x :: next) 
                       |> PriorityQueue.ofSeq |> PriorityQueue.merge dq
                   inner (Set.union seen tests) q' 
                                    
      inner (Set.singleton start) (PriorityQueue.singleton (mazeEnv.heuristic start) [start]) 

