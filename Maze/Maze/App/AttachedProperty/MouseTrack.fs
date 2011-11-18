//NO GUARANTEE. I'm not sure that using of the Event class at this pointis is allowed.
namespace FSharpWpfMvvmTemplate.AttachedProperty

open System.Windows
open System.Windows.Input

type TrackMouse  = {action : Point -> unit}

type TrackMouseBehavior() =
    let mutable startTrack = false
    let mutable endTrack = true
    static let mutable TrackPositionProperty : DependencyProperty = 
        DependencyProperty.RegisterAttached
            ("TrackPosition", typeof<TrackMouse>, typeof<TrackMouseBehavior>,
                                            new PropertyMetadata(null, new PropertyChangedCallback(TrackMouseBehavior.OnPropertyChanged)))

    static member OnPropertyChanged (d:DependencyObject) (e:DependencyPropertyChangedEventArgs) =
        let element = d :?> UIElement
        if e.NewValue <> null then
            let trackMouse = e.NewValue :?> TrackMouse
            let func = TrackMouseBehavior.MouseTrack trackMouse element
            element.PreviewMouseLeftButtonDown
               |> Event.map (fun args -> args :> MouseEventArgs)
               |> Event.merge element.PreviewMouseMove
               |> Event.filter (fun args -> args.LeftButton =  MouseButtonState.Pressed)
               |> Event.add (fun args -> func(args))

    static member GetTrackPosition(d:DependencyObject) =
        d.GetValue(TrackPositionProperty) :?> TrackMouse
    static member SetTrackPosition(d:DependencyObject, value : TrackMouse) =
        d.SetValue(TrackPositionProperty, value)
    
    static member MouseTrack(trackPos : TrackMouse) (element:UIElement) (mouseEventArgs : MouseEventArgs ) =   
            let point = mouseEventArgs.GetPosition(element)
            trackPos.action point