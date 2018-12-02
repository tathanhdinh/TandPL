// Learn more about F# at http://fsharp.org

open System

type Info = 
    | FI of string * int * int 
    | UNKNOWN

type Term = 
      TmTrue of  Info 
    | TmFalse of Info
    | TmIf of Info * Term * Term * Term
    | TmZero of Info
    | TmSucc of Info * Term
    | TmPred of Info * Term
    | TmIsZero of Info * Term
    
let rec isNumericValue t =
    match t with
        | TmZero (_) -> true
        | TmSucc (_, t1) -> isNumericValue t1
        | _ -> false


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
