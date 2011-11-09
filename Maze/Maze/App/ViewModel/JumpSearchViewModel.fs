namespace FSharpWpfMvvmTemplate.ViewModel

open System
open System.Text
open System.Xml
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.ComponentModel
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Threading
open System.Collections.ObjectModel
open Maze.JumpPointSearchType
open FSharpWpfMvvmTemplate.Model

type JumpSearchViewModel() as x=   
    class
        inherit ViewModelBase()

        let mutable gm = Geometry.Parse("")
        let mutable solverPath = Geometry.Parse("")
        let mutable animateData = String.Empty
        let mutable obstacles = set[]
        let mutable animateResult = String.Empty
        let mutable astarEnabled = false
        let mutable env = JumpMazeModel.empty
        let mutable coin = JumpMazeModel.Start (0.0, 0.0)
        let mutable timer = new DispatcherTimer(DispatcherPriority.Normal)
        let mutable animatePath = []
        let mutable undo = []
        let mutable canvas = null
        do 
            timer.Interval <- new TimeSpan(0, 0, 0, 0, 400)
            
            timer.Tick.Add(fun _  -> x.AnimateOneStep () )   
        
        interface IDataErrorInfo with
            member x.Error = String.Empty
            member x.Item
                with get(index) = 
                    x.Verify index

        member x.Verify property =
            match property with
            | prop when prop = "MazeX" -> 
                if x.MazeX < 20 then
                    "Min X = 20"
                else
                    null
            | prop when prop = "MazeY" -> 
                if x.MazeY < 20 then
                    "Min Y = 20"
                else
                    null
            | _ -> null

        static member CreateMazePath w h wallSize points =
        
            let builder = StringBuilder()
        
            let folder (acc : StringBuilder) wall  =
                match wall with
                | (x, y) ->
                    let xf, yf = (float x) * wallSize, (float y) * wallSize
                    acc.Append(sprintf "M%f,%fV%f" xf yf  (yf + wallSize))|>ignore
                    acc.Append(sprintf " H%fV%fH%f" (xf + wallSize) yf xf) 
        
            builder.Append(sprintf "M%f,%f" 0.0 0.0)|>ignore
            builder.Append(sprintf "L%f,%f %f,%f" 0.0   0.0     0.0     (h * wallSize)) |> ignore
            builder.Append(sprintf " %f,%f %f,%f" 0.0   (h * wallSize)  (w * wallSize)  (h * wallSize)) |>ignore
            builder.Append(sprintf " %f,%f %f,%f" (w * wallSize)  (h * wallSize)    (w * wallSize)    0.0) |>ignore
            builder.Append(sprintf " %f,%f %f,%f" (w * wallSize)  0.0   0.0     0.0)|>ignore
            (points |> Seq.fold folder builder).ToString()
        
        member x.AStarEnabled 
            with get () =  astarEnabled
            and set value = 
                astarEnabled <- value
                base.RaisePropertyChangedEvent(<@x.AStarEnabled@>) 

        member x.CoinX 
            with get () =  
                env.coinX
                
            and set value = 
                env <- JumpMazeModel.setCoinX env value coin
                base.RaisePropertyChangedEvent(<@x.CoinX@>) 
    
        member x.CoinY 
            with get () =  
                env.coinY
            and set value = 
                env <- JumpMazeModel.setCoinY env value coin
                base.RaisePropertyChangedEvent(<@x.CoinY@>)

        member x.TargetX 
            with get () =  
                env.targetX
                
            and set value = 
                env <- JumpMazeModel.setCoinX env value coin
                base.RaisePropertyChangedEvent(<@x.TargetX@>) 
    
        member x.TargetY 
            with get () =  
                env.targetY
            and set value = 
                env <- JumpMazeModel.setCoinY env value coin
                base.RaisePropertyChangedEvent(<@x.TargetY@>)

        member x.MazeX 
            with get () = env.maze.w
            and set value = 
                env <- JumpMazeModel.setW env value
                base.RaisePropertyChangedEvent(<@x.MazeX@>)  
    
        member x.MazeY 
            with get () =  env.maze.h
            and set value = 
                env <- JumpMazeModel.setH env value
                base.RaisePropertyChangedEvent(<@x.MazeY@>) 

        member x.MazeData  
            with get () =  gm
            and set value = 
                gm <- value
                base.RaisePropertyChangedEvent(<@x.MazeData@>) 
        
        member x.WallSize  
            with get () =  env.wallSize
            and set value = 
                env <- JumpMazeModel.setWallSize env value
                base.RaisePropertyChangedEvent(<@x.WallSize@>) 

        member x.SolverData  
            with get () =  solverPath
            and set value = 
                solverPath <- value
                base.RaisePropertyChangedEvent(<@x.SolverData@>) 
        
        member x.AnimateData  
            with get () =  Geometry.Parse(animateData)
            and set value = 
                solverPath <-  Geometry.Parse(value)
                base.RaisePropertyChangedEvent(<@x.AnimateData@>)  
        
        member x.AnimateCommand =
            new RelayCommand ((fun _ -> true), 
                                        (fun _ ->  
                                            match not env.IsEmpty && x.Verify "MazeX" = null && x.Verify "MazeY" = null with
                                            | false -> ()
                                            | true -> 
                                                x.SolverData <- Geometry.Parse("")
                                                animateData <- String.Empty
                                                x.AnimateData <- animateData
                                                List.map (fun f -> f ()) undo |> ignore
                                                coin <- JumpMazeModel.Start (env.coinX, env.coinY)
                                                let animateRun =  JumpMazeModel.run {env with obstacles = obstacles}
                                                animateResult <- animateRun |> JumpMazeModel.resultPath |> JumpMazeModel.solverToPath x.WallSize
                                                animatePath <- animateRun |> JumpMazeModel.animatePath |> Seq.toList
                                                timer.Start()))
                                                 
        member x.LeftClickCommand  = 
            new RelayCommand ((fun canExecute -> not env.IsEmpty && not timer.IsEnabled && x.Verify "MazeX" = null && x.Verify "MazeY" = null), 
                                (fun element -> 
                                    let pos = Mouse.GetPosition(element :?> UIElement)
                                    let cellPos (posx, posy) = (posx / 20.0 |> int), (posy / 20.0 |> int)
                                    match cellPos (pos.X, pos.Y) = cellPos (env.coinX, env.coinY),  cellPos (pos.X, pos.Y) = cellPos (env.targetX, env.targetY) with
                                    | true, _ -> coin <- JumpMazeModel.Start (env.coinX, env.coinY)
                                    |_, true -> coin <- JumpMazeModel.Finish (env.targetX, env.targetY)
                                    | _ ->
                                        obstacles <- Set.add (cellPos (pos.X, pos.Y)) obstacles
                                        x.MazeData <- Geometry.Parse(JumpSearchViewModel.CreateMazePath (x.MazeX |> float)  (x.MazeY |> float) x.WallSize obstacles)))
        member x.RightClickCommand  =
            new RelayCommand ((fun canExecute -> not env.IsEmpty && not timer.IsEnabled && x.Verify "MazeX" = null && x.Verify "MazeY" = null), 
                                (fun element -> 
                                    let pos = Mouse.GetPosition(element :?> UIElement)
                                    let posx, posy = (pos.X/20.0 |> int), (pos.Y / 20.0 |> int)
                                    obstacles <- Set.remove (posx, posy) obstacles
                                    x.MazeData <- Geometry.Parse(JumpSearchViewModel.CreateMazePath (x.MazeX |> float)  (x.MazeY |> float) x.WallSize obstacles)))
    
        member x.CoinMoveCommand =
            new RelayCommand ((fun canExecute -> not env.IsEmpty && not timer.IsEnabled && x.Verify "MazeX" = null && x.Verify "MazeY" = null), (fun u -> x.CoinMove(u)))
        
        member x.DrawArrow(x2, y2, d) =
            match d with
            | NONE -> ""   
            | Straight(N, _) -> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 - 3.0) (y2 + 6.0) (x2 - 3.0) (y2 + 6.0) x2 y2  x2 y2 (x2 + 3.0) (y2 + 6.0)
            | Straight(S, _) -> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 - 3.0) (y2 - 6.0) (x2 - 3.0) (y2 - 6.0) x2 y2 x2 y2 (x2 + 3.0) (y2 - 6.0)
            | Straight(E, _) -> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 - 6.0) (y2 - 3.0) (x2 - 6.0) (y2 - 3.0) x2 y2 x2 y2 (x2 - 6.0) (y2 + 3.0)
            | Straight(W, _) -> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 + 6.0) (y2 - 3.0) (x2 + 6.0) (y2 - 3.0) x2 y2 x2 y2 (x2 + 6.0) (y2 + 3.0)
            | Diagonal(NE, _)-> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 - 6.0) (y2 + 3.0) (x2 - 6.0) (y2 + 3.0) x2 y2 x2 y2 (x2 - 3.0) (y2 + 6.0)
            | Diagonal(SE, _)-> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 - 3.0) (y2 - 6.0) (x2 - 3.0) (y2 - 6.0) x2 y2 x2 y2 (x2 - 6.0) (y2 - 3.0)
            | Diagonal(SW, _)-> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 + 3.0) (y2 - 6.0) (x2 + 3.0) (y2 - 6.0) x2 y2 x2 y2 (x2 + 6.0) (y2 - 3.0)
            | Diagonal(NW, _)-> sprintf "M%f,%fL%f,%f %f,%fL%f,%f %f,%f" (x2 + 6.0) (y2 + 3.0) (x2 + 6.0) (y2 + 3.0) x2 y2 x2 y2 (x2 + 3.0) (y2 + 6.0)

        member x.CoinMove(k)= 
            env <- JumpMazeModel.moveCoin {env with obstacles=obstacles} (k :?> Key) coin
            match coin with
            | JumpMazeModel.Start _ ->
                coin <- JumpMazeModel.Start (env.coinX, env.coinY)
                base.RaisePropertyChangedEvent(<@x.CoinX@>)
                base.RaisePropertyChangedEvent(<@x.CoinY@>)
            | JumpMazeModel.Finish _ ->
                coin <- JumpMazeModel.Finish (env.targetX, env.targetY)
                base.RaisePropertyChangedEvent(<@x.TargetX@>)
                base.RaisePropertyChangedEvent(<@x.TargetY@>)


        member x.CreateMazeCommand = 
            new RelayCommand ((fun canExecute -> x.Verify "MazeX" = null && x.Verify "MazeY" = null), (fun action -> x.CreateMaze(action)))

        member x.CreateAStarCommand =
            new RelayCommand (
                                (fun canExecute -> true),
                                 (fun _ -> 
                                     if x.Verify "MazeX" = null && x.Verify "MazeY" = null then x.CreateAStar()))

        member x.CreateMaze(action) =
            if timer.IsEnabled then
                timer.Stop()
            canvas <- action :?> Canvas
            x.MazeData <- Geometry.Parse("")
            List.map (fun f -> f ()) undo|>ignore
            animateData <- String.Empty
            x.AnimateData <- animateData
            x.SolverData <- Geometry.Parse("")
            env <- JumpMazeModel.createMaze x.MazeX x.MazeY x.WallSize
            coin <- JumpMazeModel.Finish (env.targetX, env.targetY)
            x.TargetX <- env.targetX
            x.TargetY <- env.targetY
            obstacles <- env.obstacles
            gm <- Geometry.Parse(JumpSearchViewModel.CreateMazePath (x.MazeX |> float)  (x.MazeY |> float) x.WallSize obstacles)
            x.MazeData <- gm
            x.AStarEnabled <- true
        member x.AnimateOneStep () =
            match animatePath with
            | [] -> timer.Stop()
            | [((currx, curry),(x',y'), d)] ->
                let x2, y2, x1, y1 = (currx |> float) * env.wallSize + env.wallSize / 2.0, (curry |> float) * env.wallSize + env.wallSize / 2.0, (x' |> float) * env.wallSize + env.wallSize / 2.0, (y' |> float) * env.wallSize + env.wallSize / 2.0
                animateData <- sprintf "%sM%f,%fL%f,%f %f,%f%s" animateData x1 y1 x1 y1 x2 y2 (x.DrawArrow (x2, y2, d))
                x.AnimateData <- animateData
                x.SolverData <-  Geometry.Parse(animateResult)
                timer.Stop()
            | ((currx, curry),(x',y'), d) :: ts->
                let x2, y2, x1, y1 = (currx |> float) * env.wallSize + env.wallSize / 2.0, (curry |> float) * env.wallSize + env.wallSize / 2.0, (x' |> float) * env.wallSize + env.wallSize / 2.0, (y' |> float) * env.wallSize + env.wallSize / 2.0
                animateData <- sprintf "%sM%f,%fL%f,%f %f,%f%s" animateData x1 y1 x1 y1 x2 y2 (x.DrawArrow (x2, y2, d))
                x.AnimateData <- animateData
                x.CoinX <- x2
                x.CoinY <- y2
                if canvas <> null then
                    let e = new Ellipse(Width = 6.0, Height= 6.0, Fill = Brushes.Blue)
                    canvas.Children.Add(e)|>ignore
                    Canvas.SetLeft(e, x2 )
                    Canvas.SetTop(e, y2 )
                    
                    undo <- [(fun _ -> canvas.Children.Remove e;)]@undo
                animatePath <- ts
        member x.CreateAStar() =
            if timer.IsEnabled then
                timer.Stop()
            List.map (fun f -> f ()) undo|>ignore
            animateData <- String.Empty
            x.AnimateData <- animateData
            let jumpPoints = JumpMazeModel.run {env with obstacles = obstacles}
            x.SolverData <- Geometry.Parse(jumpPoints |> JumpMazeModel.resultPath |> JumpMazeModel.solverToPath x.WallSize )
    end