namespace FSharpWpfMvvmTemplate.Model

open System
open System.Windows.Input
open System.Text
open Maze.MazeType
open Maze.MazeGenerator
open Maze.UnionFind
open Astar
open Maze
open Microsoft.FSharp.Collections

module MazeModel =
    type Point = AntiObject.Point
    type MazeEnvironment = 
        { environment : AntiObject.Environment; maze : Wall list; rooms : Map<int *int, (int * int) list>; 
            w : int; h : int; wallSize : float; coinX : float; coinY : float; update : AntiObject.Environment -> AntiObject.Environment}
        member x.IsEmpty = List.isEmpty <| x.maze

    let inline flip f a b = f b a


    let empty = { environment = AntiObject.emptyEnvironment; maze = []; rooms = Map.empty;
                 w = 100; h = 100; wallSize = 20.0; coinX = 0.0; coinY = 0.0; update = id }

    let inline mapRooms mazeEnv =
        let mkWall (x, y) =
            (x,y),(List.zip [(-1,0); (0,-1); (1,0); (0, 1)] [V(x-1,y); H(x,y-1); V(x,y); H(x,y)]) 
            |>List.filter (not << flip List.exists mazeEnv.maze << (=) <<snd)
            |>List.map fst
        PSeq.map mkWall [for x in [0..mazeEnv.w] do
                         for y in [0..mazeEnv.h] do
                         yield x,y] |> PSeq.toList |> Map.ofList 
    
    let createSolver mazeEnv = 
        let startx, starty = (int mazeEnv.coinX) / int mazeEnv.wallSize, (int mazeEnv.coinY) / int mazeEnv.wallSize
        match mazeEnv.w > 0 && mazeEnv.h > 0, Map.isEmpty mazeEnv.rooms with 
        | false, _      -> [] 
        | true, true    -> MazeSolver.run (mapRooms mazeEnv) ((startx, starty), (mazeEnv.w - 1, mazeEnv.h - 1)) mazeEnv.w mazeEnv.h AstarImpl.astar
        | true, false   -> MazeSolver.run mazeEnv.rooms ((startx, starty), (mazeEnv.w - 1, mazeEnv.h - 1)) mazeEnv.w mazeEnv.h AstarImpl.astar

    let inline fupdate w h =
        [for x in [-1..w] do
                    for y in [-1..h] do
                    yield (x,y)]
        |> AntiObject.update

    let moveCoin mazeEnv move =
        let moveX, moveY =
            let cx,cy = (int mazeEnv.coinX) / int mazeEnv.wallSize , (int mazeEnv.coinY) / int mazeEnv.wallSize
            match move, mazeEnv.IsEmpty with
            | _, true -> mazeEnv.coinX, mazeEnv.coinY
            | Key.Down, false ->             
                if cy >= mazeEnv.h - 1 || (List.exists ( fun w -> w = H(cx,cy)) mazeEnv.maze ) then
                    mazeEnv.coinX, mazeEnv.coinY
                else
                    mazeEnv.coinX, mazeEnv.coinY + mazeEnv.wallSize
            | Key.Up, false -> 
                if cy = 0 || (List.exists ( fun w -> w = H(cx, cy - 1)) mazeEnv.maze) then
                    mazeEnv.coinX, mazeEnv.coinY
                else
                    mazeEnv.coinX, mazeEnv.coinY - mazeEnv.wallSize
            | Key.Right, false -> 
                if cx >= mazeEnv.w-1 || (List.exists ( fun w -> w = V(cx, cy)) mazeEnv.maze) then
                    mazeEnv.coinX, mazeEnv.coinY
                else
                    mazeEnv.coinX + mazeEnv.wallSize, mazeEnv.coinY
            | Key.Left, false -> 
                if cx = 0 || (List.exists ( fun w -> w = V(cx-1, cy)) mazeEnv.maze) then
                    mazeEnv.coinX, mazeEnv.coinY
                else
                    mazeEnv.coinX - mazeEnv.wallSize, mazeEnv.coinY
            | _, false -> mazeEnv.coinX, mazeEnv.coinY

        if (moveX, moveY) <> (mazeEnv.coinX, mazeEnv.coinY) then
            let goalX, goalY = (int moveX) / int mazeEnv.wallSize, (int moveY) / int mazeEnv.wallSize
            match AntiObject.moveGoal (goalX, goalY) mazeEnv.environment with
            | e, true -> 
                {mazeEnv with environment = mazeEnv.update e; coinX = moveX; coinY = moveY}
            | e, false -> {mazeEnv with environment = mazeEnv.update e}
        else
            {mazeEnv with environment = mazeEnv.update mazeEnv.environment}

    let mazeToPath w h mazeEnv =
        let builder = StringBuilder()
        
        let folder (acc : StringBuilder) wall  =
            match wall with
            | H(x, y) ->
                let xf, yf = (float x) * mazeEnv.wallSize, (float y) * mazeEnv.wallSize
                acc.Append(sprintf "M%f,%fH%f" xf (yf + mazeEnv.wallSize)  (xf + mazeEnv.wallSize))
            | V(x, y) ->
                let xf, yf =(float x) * mazeEnv.wallSize, (float y) * mazeEnv.wallSize
                acc.Append(sprintf "M%f,%fV%f" (xf + mazeEnv.wallSize) yf (yf + mazeEnv.wallSize))

        
        builder.Append(sprintf "M%f,%f" 0.0 0.0)|>ignore
        builder.Append(sprintf "L%f,%f %f,%f" 0.0   0.0     0.0     (h * mazeEnv.wallSize)) |> ignore
        builder.Append(sprintf " %f,%f %f,%f" 0.0   (h * mazeEnv.wallSize)  (w * mazeEnv.wallSize)  (h * mazeEnv.wallSize)) |>ignore
        builder.Append(sprintf " %f,%f %f,%f" (w * mazeEnv.wallSize)  (h * mazeEnv.wallSize)    (w * mazeEnv.wallSize)    0.0) |>ignore
        builder.Append(sprintf " %f,%f %f,%f" (w * mazeEnv.wallSize)  0.0   0.0     0.0)|>ignore
        
        (mazeEnv.maze |> PSeq.fold folder builder).ToString()

    let inline solverToPath wallSize solver =
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
    
    let createMaze w h l = 
        {environment = AntiObject.emptyEnvironment; w = w; h = h; wallSize = l; maze = MazeGenerator.genMaze w h; 
            rooms = Map.empty; coinX = 0.0; coinY = 0.0; update = fupdate w h}
    
    let isBoardEmpty mazeEnv = 
        Map.isEmpty mazeEnv.environment.board 

    let createEnvironment mazeEnv desirability rate =
        let sx, sy = (int mazeEnv.coinX) / int mazeEnv.wallSize, (int mazeEnv.coinY) / int mazeEnv.wallSize
        let fcreate rooms = 
            AntiObject.createEnvironment mazeEnv.w mazeEnv.h rooms (desirability, sx, sy) (0, mazeEnv.h - 1) ((mazeEnv.w - 1) / 2, (mazeEnv.h-1) / 2) rate
            
        match Map.isEmpty mazeEnv.rooms with
        | true ->
            let rooms = (mapRooms mazeEnv)
            {mazeEnv with rooms = rooms; coinX = (mazeEnv.wallSize / 4.0); coinY = (mazeEnv.wallSize / 4.0); environment = fcreate rooms}
        | false ->
            {mazeEnv with coinX = (mazeEnv.wallSize / 4.0); coinY = (mazeEnv.wallSize / 4.0); environment = fcreate mazeEnv.rooms}


    let enemiesPos mazeEnv = 
        mazeEnv.environment.pursuers 
        |> List.map (fun (x, y) ->  
                float x * mazeEnv.wallSize + (mazeEnv.wallSize / 4.0), float y * mazeEnv.wallSize + (mazeEnv.wallSize / 4.0))
    
    let setW mazeEnv w = {mazeEnv with w = w}

    let setH mazeEnv h = {mazeEnv with h = h}
    
    let setCoinX mazeEnv x = 
        if x < float (mazeEnv.w * int mazeEnv.wallSize) && List.isEmpty mazeEnv.maze |> not then
                {mazeEnv with coinX = x}
        else
            mazeEnv
    
    let setCoinY mazeEnv y = 
        if y < float (mazeEnv.h * int mazeEnv.wallSize) && List.isEmpty mazeEnv.maze |> not then
            {mazeEnv with coinY = y}
        else
            mazeEnv

    let setWallSize mazeEnv l = {mazeEnv with wallSize = l}


