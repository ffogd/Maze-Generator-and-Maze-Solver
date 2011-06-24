// Weitere Informationen zu F# unter "http://fsharp.net".

// Astar.fs
//from Haskell version http://www.haskell.org/haskellwiki/Haskell_Quiz/Astar/Solution_Dolio
namespace Astar
 
module AstarTypes =
    type Point = int * int
    type Map = char list list
 
    let inline flip f b a = f a b
 
[<RequireQualifiedAccess>]
module PriorityQueue =
  exception Empty
 
  type t<'k,'a> =
    | E
    | T of 'k * 'a * t<'k, 'a> * Lazy<t<'k,'a>>
 
  let empty = E
 
  let isEmpty = function E -> true | _ -> false
 
  let inline singleton prio x = T(prio, x, E, lazy E)
 
  let rec merge t1 t2 =
    match t1, t2 with
    | E, h -> h
    | h, E -> h
    | T(xprio, _, _, _), T(yprio, _, _, _) ->
        if xprio <= yprio then link t1 t2 else link t2 t1
 
  and link t1 t2 =
    match t1, t2 with
    | T(prio, a, E, m), r -> T(prio, a, r, m)
    | T(prio, a, t, m), r -> T(prio, a, E, lazy merge (merge r t) (m.Force()))
    | _ -> failwith "should not get there"
 
  let inline insert prio x q = merge (singleton prio x) q
 
  let rec contains prio = function
    | E -> false
    | T (sndPrio, _, a, b) ->
        prio = sndPrio || contains prio a || contains prio (b.Force())
 
  let deleteFindMin = function
    | E -> raise Empty
    | T(prio, a, t, m) ->(prio, a), merge t (m.Force())
   
  let inline findMin q = fst (deleteFindMin q)
 
  let inline deleteMin q = snd (deleteFindMin q)
 
 
  let rec remove x = function
    | E -> E
    | T(prio, y, a, b) as t ->
        if a = x
        then merge a (b.Force())
        else T(prio, y, remove x a, lazy remove x (b.Force()))
 
  let inline ofSeq s = Seq.fold (fun q (prio, a) -> merge (singleton prio a) q) empty s
 
module AstarImpl = 
  open Microsoft.FSharp.Collections
  //Point -> (Point -> Set<Point>) -> (Point -> bool) -> (Point -> int) -> (Point -> int) -> Point list
  let astar start succ finish cost heur =
      let rec inner seen q =
           match PriorityQueue.isEmpty q with
           | true -> failwith "No Solution."
           | false ->
               let ((c, next), dq) = PriorityQueue.deleteFindMin q
               let n = List.head next
 
               match finish n with
               | true -> next
               | otherwise -> 
                   let succs = succ n
 
                   let costs item = c + (cost item) + (heur item) - (heur n) 
                    
                   let q' = 
                       Set.difference succs seen |> PSeq.map (fun x -> costs x, x :: next) 
                       |> PriorityQueue.ofSeq |> PriorityQueue.merge dq
 
                   inner (Set.union seen succs) q'
      inner (Set.singleton start) (PriorityQueue.singleton (heur start) [start])

