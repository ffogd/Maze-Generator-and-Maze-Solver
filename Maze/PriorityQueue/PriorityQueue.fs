namespace Astar

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

