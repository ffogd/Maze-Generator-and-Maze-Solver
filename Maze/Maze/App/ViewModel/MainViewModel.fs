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


type MainViewModel() =   
    class
        inherit ViewModelBase()
        
        member x.MazeCommand = 
            new RelayCommand ((fun canExecute -> true), (fun action -> x.MazeWindow()))
        member x.MazeWindow() =
            let mazeWin = Application.LoadComponent(
                                    new System.Uri("/App;component/Maze.xaml", UriKind.Relative)) :?> Window
            mazeWin.ShowDialog() |> ignore
        member x.JumpPointCommand = 
            new RelayCommand ((fun canExecute -> true), (fun action -> x.JumpPointWindow()))
        member x.JumpPointWindow() =
            let window = Application.LoadComponent(
                                    new System.Uri("/App;component/JumpSearch.xaml", UriKind.Relative)) :?> Window
            window.ShowDialog() |> ignore
end