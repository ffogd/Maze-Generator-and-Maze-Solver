// Weitere Informationen zu F# unter "http://fsharp.net".

// Astar.fs
//from Haskell version http://www.haskell.org/haskellwiki/Haskell_Quiz/Astar/Solution_Dolio
namespace Astar
 
module AstarTypes =
    type Point = int * int
    type Map = char list list
 
    let inline flip f b a = f a b
 

 
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

