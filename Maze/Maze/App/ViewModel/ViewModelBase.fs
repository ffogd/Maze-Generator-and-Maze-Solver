namespace FSharpWpfMvvmTemplate.ViewModel

open System
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type ViewModelBase() =
    let propertyChangedEvent = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    member x.RaisePropertyChangedEvent (expr: Expr) =
        match expr with
        | PropertyGet(_, methodInfo, _) ->
            let propertyName = methodInfo.Name
            propertyChangedEvent.Trigger(x, new PropertyChangedEventArgs(propertyName))
        | other -> failwith "not implemented" 
        
