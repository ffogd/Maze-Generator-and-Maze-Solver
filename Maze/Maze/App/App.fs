module MainApp

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open FSharpWpfMvvmTemplate.ViewModel


//let mainWindowViewModel = Application.LoadComponent(
//                             new System.Uri("/App;component/mainwindow.xaml", UriKind.Relative)) :?> Window
//mainWindowViewModel.DataContext <- new MainWindowViewModel() 
// Create the View and bind it to the View Model
let window =
    let mainWindowViewModel = Application.LoadComponent(
                                    new System.Uri("/App;component/Main.xaml", UriKind.Relative)) :?> Window
//                                 new System.Uri("/FSharpWpfMvvmTemplate.App;component/mainwindow.xaml", UriKind.Relative)) :?> Window
    mainWindowViewModel
// Application Entry point
[<STAThread>]
[<EntryPoint>]
let main(_) = (new Application()).Run(window)
