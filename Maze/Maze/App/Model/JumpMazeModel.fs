namespace FSharpWpfMvvmTemplate.Model

open System
open System.Windows.Input
open System.Text
open Microsoft.FSharp.Collections


module JumpMazeModel =
    open Maze.MazeType
    open Maze.MazeUtils
    open Maze.MazeGenerator
    open Maze.UnionFind
    open Maze.JumpPointSearchType
    open Maze.JumpPointsSearch

    type SelectedCoin =
        | Start of float * float
        | Finish of float * float

    type MazeEnvironment = 
        { maze : JumpPointEnvironment; obstacles : Set<(int * int)>; 
          wallSize : float; coinX : float; coinY : float; targetX : float; targetY : float}
        member this.IsEmpty = Map.isEmpty <| this.maze.grid

    let empty = { maze = empty; obstacles = Set.empty;
                  wallSize = 20.0; coinX = 0.0; coinY = 0.0;targetX = 0.0; targetY = 0.0 }
    // Create the grid from the obstacles set.
    // mapObstaclesToGrid : JumpPointEnvironment -> Set<int * int> -> JumpPointEnvironment
    let inline mapObstaclesToGrid mazeEnv obstacles =
        
        let notObstacleDiagonal straight pos =
            let isInGrid = List.forall (inGrid mazeEnv.w mazeEnv.h)  (pos :: straight)
            match isInGrid with
            | true -> not <| Set.contains pos obstacles && Set.intersect (Set.ofList straight) obstacles |> Set.count < 2
            | _  ->   false

        let notObstacle cell = 
            match inGrid  mazeEnv.w mazeEnv.h cell with
            | true -> not <| Set.contains cell obstacles 
            | false -> false

        let mkWall (x, y) =
            let add =  addPoint (x, y)
            (x,y), [straight W,   straightPosition W |> add |> notObstacle;
                    straight N,   straightPosition N |> add |> notObstacle;
                    straight E,   straightPosition E |> add |> notObstacle; 
                    straight S,   straightPosition S |> add |> notObstacle; 
                    diagonal NW,  diagonalPosition NW |> add |> notObstacleDiagonal [straightPosition N |> add; 
                                                                                     straightPosition W |> add ]; 
                    diagonal NE,  diagonalPosition NE |> add |> notObstacleDiagonal [straightPosition N |> add;
                                                                                     straightPosition E |> add ]; 
                    diagonal SW,  diagonalPosition SW |> add |> notObstacleDiagonal [straightPosition S |> add;
                                                                                     straightPosition W |> add ]; 
                    diagonal SE,  diagonalPosition SE |> add |> notObstacleDiagonal [straightPosition S |> add;
                                                                                     straightPosition E |> add ]]
            |> List.filter (id << snd)
            |> List.map fst
        {mazeEnv with 
            grid = Seq.map mkWall 
                        [ for x in [0..mazeEnv.w-1] do
                            for y in [0..mazeEnv.h-1] do
                            yield x, y] |> Seq.toList |> Map.ofList }
    
    // run : MazeEnvironment -> seq<(int * int) * ((int * int) * float)>    
    let run env = 
        let jumpPointEnv = mapObstaclesToGrid env.maze env.obstacles
        let start = env.coinX / env.wallSize |> int, env.coinY / env.wallSize |>int
        let finish = env.targetX / env.wallSize |> int, env.targetY / env.wallSize |> int
        astarJump start { jumpPointEnv with isGoal = ((=) finish); stepCosts = stepCosts;  heuristic = (diagHeuristic finish) }

    // jump points  seq<jumpPoint   * (parent      * cost)>  to path of points list.
    // resultPath : seq<(int * int) * ((int * int) * float)> -> (int * int) list
    let inline resultPath jumpPoints =
        jumpPoints
        |> Seq.groupBy (fst)
        |> Seq.map (fun (key, s)-> key, Seq.minBy (snd << snd) s |> snd |> fst) 
        |> Seq.toList |> List.rev
        |> List.fold (fun acc (curr, parent) -> 
                        match acc with
                        | [] -> [parent;curr;]
                        | x :: _ when x = curr-> parent :: acc
                        | _ -> acc) []
    
    let inline animatePath jumpPoints = jumpPoints |> Seq.map (fun (curr, (parent, _)) -> curr, parent, directionToParent curr parent)

    let generateJumpObstacle w h size =
        let initMaze = genMaze (w-1) (h-1)
        initMaze
        |>List.map 
            (fun wall -> 
                match wall with
                | V(x, y) -> [for s in 0..(size-1) -> (x+1)*size - 1, (y+1)*size - 1 -  s]
                | H(x, y) -> [for s in 0..(size-1) -> (x+1)*size - 1 - s, (y+1)*size - 1 ])
        |>List.concat

    let createMaze w h wallSize = 
        let maze = {Maze.JumpPointSearchType.empty with w = w; h = h}
        let obstacles = 
                generateJumpObstacle 5 5 5
                |> Set.ofList
        let rooms = mapObstaclesToGrid maze obstacles
        { maze = rooms; obstacles = obstacles;
          wallSize = wallSize; 
          coinX = 0.0; coinY = 0.0; 
          targetX = ((w - 1) |> float) * wallSize; 
          targetY = ((h - 1) |> float) * wallSize}
        
    let animateToPath wallSize animatePoints =
        match Seq.isEmpty animatePoints with
        | false ->
            let builder = StringBuilder()
            let (current, (xstart, ystart)) = Seq.head animatePoints
            
            builder.Append(sprintf "M%f,%f" ((float xstart) * wallSize + wallSize / 2.0) ((float ystart) * wallSize + wallSize / 2.0))|>ignore
            let folder (acc : StringBuilder) ((x, y), (x',y')) =
                let xf, yf =    (float x) * wallSize, (float y) * wallSize
                let xf', yf' =  (float x') * wallSize, (float y') * wallSize 
                acc.Append(sprintf "M%f,%fL%f,%f %f,%f" (xf' + wallSize / 2.0)  (yf' + wallSize / 2.0) (xf' + wallSize / 2.0) (yf' + wallSize / 2.0)  (xf + wallSize / 2.0)  (yf + wallSize / 2.0))

            (animatePoints
             |> PSeq.fold folder builder).ToString()
        | true -> String.Empty

    let solverToPath wallSize solver =
        match Seq.isEmpty solver with
        | false ->
            let builder = StringBuilder()
            let (xstart, ystart) = Seq.head solver

            builder.Append(sprintf "M%f,%f" ((float xstart) * wallSize + wallSize / 2.0) ((float ystart) * wallSize + wallSize / 2.0))|>ignore

            let folder (acc : StringBuilder) ((x, y), (x',y')) =
                let xf, yf =    (float x) * wallSize, (float y) * wallSize
                let xf', yf' =  (float x') * wallSize, (float y') * wallSize 
                acc.Append(sprintf "L%f,%f %f,%f" (xf + wallSize / 2.0) (yf + wallSize / 2.0)  (xf' + wallSize / 2.0)  (yf' + wallSize / 2.0))

            (solver
            |> Seq.pairwise
            |> PSeq.fold folder builder).ToString()
        | true -> String.Empty
                
    let moveCoin (mazeEnv : MazeEnvironment) move selectedCoin =
        let inner (cx, cy) (coinX, coinY) =
            match move, mazeEnv.IsEmpty with
            | _, true -> coinX, coinY
            | Key.Down, false ->             
                if cy >= mazeEnv.maze.h - 1 || (Set.exists ( fun w -> w = (cx, cy + 1)) mazeEnv.obstacles ) then
                    coinX, coinY
                else
                    coinX, coinY + mazeEnv.wallSize
            | Key.Up, false -> 
                if cy = 0 || (Set.exists ( fun w -> w = (cx, cy - 1)) mazeEnv.obstacles) then
                    coinX, coinY
                else
                    coinX, coinY - mazeEnv.wallSize
            | Key.Right, false -> 
                if cx >= mazeEnv.maze.w-1 || (Set.exists ( fun w -> w = (cx + 1, cy)) mazeEnv.obstacles) then
                    coinX, coinY
                else
                    coinX + mazeEnv.wallSize, coinY
            | Key.Left, false -> 
                if cx = 0 || (Set.exists ( fun w -> w = (cx-1, cy)) mazeEnv.obstacles) then
                    coinX, coinY
                else
                    coinX - mazeEnv.wallSize, coinY
        match selectedCoin with
        | Start (dx, dy) -> 
            let cx, cy = (int dx) / int mazeEnv.wallSize , (int dy) / int mazeEnv.wallSize
            let moveX, moveY = inner (cx, cy) (dx, dy)
            {mazeEnv with coinX = moveX; coinY = moveY}
        | Finish (dx, dy) -> 
            let cx, cy = (int dx) / int mazeEnv.wallSize , (int dy) / int mazeEnv.wallSize
            let moveX, moveY = inner (cx, cy) (dx, dy)
            {mazeEnv with targetX = moveX; targetY = moveY}
        
    let setW mazeEnv w = {mazeEnv with maze = {mazeEnv.maze with w = w}}

    let setH mazeEnv h = {mazeEnv with maze = {mazeEnv.maze with h = h}}
    
    let setCoinX (mazeEnv : MazeEnvironment) x selectedCoin = 
        if mazeEnv.IsEmpty |> not && x < float (mazeEnv.maze.w * int mazeEnv.wallSize)  then
               match selectedCoin with
               | Start _ -> {mazeEnv with coinX = x}
               | Finish _ -> {mazeEnv with targetX = x}
        else
            mazeEnv
    
    let setCoinY (mazeEnv : MazeEnvironment) y selectedCoin = 
        if  mazeEnv.IsEmpty |> not && y < float (mazeEnv.maze.h * int mazeEnv.wallSize) then
            match selectedCoin with
               | Start _ -> {mazeEnv with coinY = y}
               | Finish _ -> {mazeEnv with targetY = y}
        else
            mazeEnv

    let setWallSize mazeEnv l = {mazeEnv with wallSize = l}