namespace FSharpWpfMvvmTemplate.ViewModel

open System
open System.Text
open System.Xml
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.ComponentModel
open System.Collections.ObjectModel
open FSharpWpfMvvmTemplate.Model

type MazeViewModel() =   
    inherit ViewModelBase()

    let mutable gm = Geometry.Parse("")
    let mutable solverPath = Geometry.Parse("")

    let mutable enemy1X = -20.0
    let mutable enemy1Y = -20.0
    let mutable enemy2X = -10.0
    let mutable enemy2Y = -10.0
    let mutable rate    = 0.1
    let mutable desirability = 20.0

    let mutable env = MazeModel.empty

    member x.SetEnemiesPos () = 
        List.zip 
            [(fun (posX, posY) -> x.Enemy1X <- posX; x.Enemy1Y <- posY); (fun (posX, posY) -> x.Enemy2X <- posX; x.Enemy2Y <- posY)] 
            (MazeModel.enemiesPos env)
        |> List.map (fun (f, arg) -> f arg) |> ignore
    member x.Enemy1X
        with get () =  enemy1X
        and set value = 
            enemy1X <- value
            base.RaisePropertyChangedEvent(<@x.Enemy1X@>) 

    member x.Enemy1Y
        with get () =  enemy1Y
        and set value = 
            enemy1Y <- value
            base.RaisePropertyChangedEvent(<@x.Enemy1Y@>) 
    
    member x.Enemy2X
        with get () =  enemy2X
        and set value = 
            enemy2X <- value
            base.RaisePropertyChangedEvent(<@x.Enemy2X@>) 

    member x.Enemy2Y
        with get () =  enemy2Y
        and set value = 
            enemy2Y <- value
            base.RaisePropertyChangedEvent(<@x.Enemy2Y@>) 

    member x.CoinX 
        with get () =  env.coinX
        and set value = 
            env <- MazeModel.setCoinY env value
            base.RaisePropertyChangedEvent(<@x.CoinX@>) 
    
    member x.CoinY 
        with get () =  env.coinY
        and set value = 
            env <- MazeModel.setCoinY env value
            base.RaisePropertyChangedEvent(<@x.CoinY@>)

    member x.MazeX 
        with get () = env.w
        and set value = 
            env <- MazeModel.setW env value
            base.RaisePropertyChangedEvent(<@x.MazeX@>)  
    
    member x.MazeY 
        with get () =  env.h
        and set value = 
            env <- MazeModel.setH env value
            base.RaisePropertyChangedEvent(<@x.MazeY@>) 

    member x.MazeData  
        with get () =  gm
        and set value = 
            gm <- value
            base.RaisePropertyChangedEvent(<@x.MazeData@>)  

    member x.Rate  
        with get () =  rate
        and set value = 
            rate <- value
            base.RaisePropertyChangedEvent(<@x.Rate@>) 
    
    member x.WallSize  
        with get () =  env.wallSize
        and set value = 
            env <- MazeModel.setWallSize env value
            base.RaisePropertyChangedEvent(<@x.WallSize@>) 

    member x.Desirability  
        with get () =  desirability
        and set value = 
            desirability <- value
            base.RaisePropertyChangedEvent(<@x.Desirability@>) 

    member x.SolverData  
        with get () =  solverPath
        and set value = 
            solverPath <- value
            base.RaisePropertyChangedEvent(<@x.SolverData@>) 
    
    member x.CoinMoveCommand =
        new RelayCommand ((fun canExecute -> true), (fun u -> x.CoinMove(u)))
    
    member x.CoinMove(k)= 
        if not env.IsEmpty then
            if MazeModel.isBoardEmpty env then
                x.Play()
            else
                env <- MazeModel.moveCoin env (k:?>Key)
                x.SetEnemiesPos ()
                base.RaisePropertyChangedEvent(<@x.CoinX@>)
                base.RaisePropertyChangedEvent(<@x.CoinY@>)

    member x.CreateMazeCommand = 
        new RelayCommand ((fun canExecute -> true), (fun action -> x.CreateMaze()))

    member x.CreateAStarCommand =
        new RelayCommand ((fun canExecute -> true), (fun _ -> x.CreateAStar()))
    
    member x.CreatePlayCommand =
        new RelayCommand ((fun canExecute -> true), (fun _ -> x.Play()))
    
    member x.Play() =
        env <- MazeModel.createEnvironment env desirability rate
        x.SetEnemiesPos ()
        base.RaisePropertyChangedEvent(<@x.CoinX@>)
        base.RaisePropertyChangedEvent(<@x.CoinY@>)
        
    //MazeViewModel.fs
    member x.CreateMaze() =
        x.SolverData <- Geometry.Parse("")
        env <- MazeModel.createMaze x.MazeX x.MazeY x.WallSize
        x.MazeData <- Geometry.Parse(MazeModel.mazeToPath (float x.MazeX) (float x.MazeY) env)


    member x.CreateAStar() =
        x.SolverData <- Geometry.Parse(MazeModel.solverToPath x.WallSize (MazeModel.createSolver env))