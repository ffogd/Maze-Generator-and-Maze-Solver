// Diese Datei ist ein Skript, das mit F# interaktiv ausgeführt werden kann.  
// Es kann zur Erkundung und zum Testen des Bibliotheksprojekts verwendet werden.
// Skriptdateien gehören nicht zum Projektbuild.

#load "Module1.fs"
open System

let quickSort list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | x::[] -> x::acc
        | x::xs ->
            let l, r = List.partition ((>) x) xs
            let rs = loop r acc
            loop l (x::rs)
    loop list []

let quickSort1 list =
    let s = System.Collections.Generic.Stack[list]
    let rec loop (stack:Collections.Generic.Stack<'a list>) acc =
        match stack.Count>0 with
        | false -> acc
        | true ->
            match stack.Pop() with
            | [] -> loop stack acc
            | x :: [] -> loop stack (x::acc)
            | x :: y :: []-> 
                if x > y then
                   loop stack (y :: x :: acc) 
                else
                   loop stack (x::y::acc) 
            | x::xs ->
                let l, r = List.partition ((>) x) xs
                stack.Push l
                stack.Push [x]
                stack.Push r
                loop stack acc
    loop s [];;
let quickLoopCont l =
    let rec inner qlist acc =        
        match qlist with
        | [] -> acc []
        | small when small.Length < 16 ->
            acc (List.sort small)
        | p::tail -> 
            let (lower, upper) = List.partition ((>=) p) tail
            inner lower (fun loweracc ->
            inner upper (fun upperacc -> acc (List.append (loweracc) (p :: upperacc))))

    // Perform the sorting
    inner l id 
let quickSort2 list =
    let s = System.Collections.Generic.Stack[list]
    let rec loop (stack:Collections.Generic.Stack<'a list>) acc =
        match stack.Count>0 with
        | false -> acc
        | true ->
            match stack.Pop() with
            | [] -> loop stack acc
            | small when small.Length < 16 -> loop stack (List.append (List.sort small) acc)
            | x::[] -> loop stack (x::acc)
            | x::xs ->
                let l, r = List.partition ((>) x) xs
                stack.Push l
                stack.Push [x]
                stack.Push r
                loop stack acc
    loop s []

let rand = new System.Random()
let data = List.init 800000 (fun _ -> rand.NextDouble())

let test f x =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    f x |> ignore
    sw.Stop()
    sw.ElapsedMilliseconds

printf "quickSort: %dms\n" (test quickSort data)
printf "quickSort1: %dms\n" (test quickSort1 data)
printf "List.sort: %dms\n" (test List.sort data)
printf "quickLoopCont: %dms\n" (test quickLoopCont data)
//    let rec loop list acc =
//        match list with
//        | [] -> acc
//        | x::[] -> x::acc
//        | x::xs ->
//            let l, r = List.partition ((>) x) xs
//            let rs = loop r acc
//            loop l (x::rs)
//    loop list []