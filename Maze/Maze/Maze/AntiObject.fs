//AntiObject.fs
namespace Maze
open System

module CustomStack =
    exception Empty

    type CustomStack<'a> =
        | Nil
        | Cons of ('a * CustomStack<'a>)
    
    let empty = Nil
    
    let isEmpty = function Nil -> true | _ -> false

    let cons x cs = Cons(x, cs)

    let singleton x = cons x empty

    let head = function
        | Nil -> raise Empty
        | Cons (hd, tl) -> hd

    let tail = function
        | Nil -> raise Empty
        | Cons (hd, tl) -> tl
    
    let rec append x y =
        match x with
        | Nil -> y
        | Cons (hd, tl) -> Cons (hd, append tl y)

    let rec set xs i x =
        match xs, i with
        | Nil, _ -> raise Empty
        | Cons (hd, tl), 0 -> Cons(x, tl)
        | Cons(hd, tl), n -> Cons(hd, set tl (i-1) x)



module AntiObject =
    open CustomStack
    open MazeType
    open MazeUtils
    open UnionFind
    open Microsoft.FSharp.Collections

    let inline flip f a b = f b a

    let rec removeOne value list = 
        match list with
        | head::tail when head = value -> tail
        | head::tail -> head::(removeOne value tail)
        | _ -> []
    
    type Either<'a,'b> =
        | Left of 'a
        | Right of 'b

    type Point = int * int

    type Agent = 
        | Goal of Double
        | Pursuer
        | Path of Double
        | Obstacle

    type Environment = {board : Map<Point, CustomStack<Agent>>; w : int; h : int; pursuers : Point list; goal : Point; 
                        rooms: Map<(int * int),(int * int) list> ; rate : double}

    let emptyEnvironment = {board = Map.empty; w = 0; h = 0; pursuers = []; goal= (0, 0); rooms = Map.empty; rate = 0.0 }


    let inline scent agent =
        match agent with
        | Path s    -> s
        | Goal s    -> s
        | _         -> 0.0

    let inline zeroScent agent =
        match agent with
        | Path s -> Path 0.0
        | x      -> x

    let inline zeroScents agents =
        match agents with
        | Cons(x, xs) -> cons (zeroScent x)  xs
        | x           -> x

    let inline topScent agents =
        match agents with
        | Cons(x, _) -> scent x
        | _          -> 0.0

    //Builds a basic environment
    //createEnvironment :: int -> -> int -> Map<(int * int),(int * int) list [,] -> (float * int * int) -> int * int -> int * int -> float- > Environment
    let inline createEnvironment w h rooms (goal, xgoal, ygoal) (xpursuer1, ypursuer1) (xpursuer2,ypursuer2) rate = 
        let mkAgent x y =
            let path = singleton (Path 0.0)
            match x, y with
            | x, y when x = -1 || y = -1 || x = w || y = h  -> singleton Obstacle
            | x, y when x = xgoal && y = ygoal              -> cons (Goal goal)  path
            | x, y when x = xpursuer1 && y = ypursuer1      -> cons Pursuer       path
            | x, y when x = xpursuer2 && y = ypursuer2      -> cons Pursuer       path
            | otherwise                                     -> path
        let b = Map.ofList [for y in -1..h do
                            for x in -1..w do
                            yield ((x, y), mkAgent x y)]
        {board = b; w = w; h = h; pursuers = [(xpursuer1, ypursuer1); (xpursuer2,ypursuer2)]; goal =(xgoal, ygoal); rooms = rooms; rate = rate}

    //canMove :: CustomStack<Agent> option -> bool
    let inline canMove someAgents =
        match someAgents with
        | Some (Cons(Path _, _))    -> true
        | _                         -> false

    //move :: Map<Point, CustomStack<Agent>> -> Point -> Point -> Map<Point, CustomStack<Agent>>
    let inline move (e : Map<Point, CustomStack<Agent>>) src tgt =
        let (Cons(h, tl)) = e.[src]   
        e
        |> Map.add tgt (cons h e.[tgt])
        |> Map.add src (zeroScents tl)
    
    //moveGoal :: Point -> Environment -> Environment * bool
    let inline moveGoal dest e =
        let targetSuitable = canMove (Map.tryFind dest e.board)
        match targetSuitable with
        | true -> {e with board = move e.board e.goal dest
                                        ; goal = dest }, true
        | false -> e, false

    let inline checkPoint p e board =
        let mapper p (dx,dy)  =
            Map.tryFind (addPoint p (dx, dy)) board

        match p with
        | x, y when x < 0 || y < 0  -> List.empty
        | _                         -> e.rooms.[p] |> List.map (mapper p)
             
    // Ensure we only move if there is a better scent available
    //updatePursuer :: Environment -> Point -> Environment
    let inline updatePursuer e p =
        let top = topScent<<flip Map.find e.board
        let neighbours = 
            e.rooms.[p] 
            |> List.map (addPoint p)
            |> List.filter (canMove<<flip Map.tryFind e.board)
            |> List.filter (flip (>=) (top p)<<top) 
        match neighbours with
        | []  -> e
        | _   -> 
            let tgt = List.maxBy (scent<<head<<flip Map.find e.board) neighbours
            {e with board = move e.board p tgt;
                    pursuers = tgt :: removeOne p e.pursuers }

    //diffusePoint :: float -> CustomStack<Agent> -> Agent list -> CustomStack<Agent>
    let inline diffusePoint rate agents check = 
        let diffusedScent s ys = s + rate * List.sum (List.map (fun x -> (scent x) - s) ys)
    
        let diffuse agents n  =
            match agents with
            | Cons (Path d, r) -> cons (Path  (diffusedScent d n )) r
            | other            -> other   
         
        let neighbours =                 
            match check with
            | _ :: _ ->   List.map head (check |> List.choose id )
            | [] ->  List.empty
    
        diffuse agents neighbours  


    //updatePursuers :: Environment -> Environment
    let inline updatePursuers env = Seq.fold updatePursuer env (env.pursuers)

    // update :: Point seq -> Environment -> Environment
    let inline update boardPoints e = 
        let updateBoard = 
            PSeq.fold (fun acc p ->
                        let dp = diffusePoint e.rate e.board.[p] (checkPoint p e acc)
                        Map.add p dp acc) e.board
        updatePursuers {e with board = updateBoard boardPoints}      

