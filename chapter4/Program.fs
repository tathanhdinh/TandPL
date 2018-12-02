module Chapter4

(*
    Each node of AST is attached with some information
      - message about parsed token
      - possition of the token in source file
*)
type Info =
| Fi of string * int // File information
| Unknown

(* 
    // terms
    t ::= 
        | true | false | if t then t else t
        | 0 | succ t | pred t | iszero t
    
    // values: bool values + numerical values
    v ::= 
        | true | false 
        | nv

    // numerical values
    nv ::= 
        | 0 | succ nv
*)
type Term = 
| TmTrue of Info
| TmFalse of Info
| TmIf of Info * Term * Term * Term
| TmZero of Info
| TmSucc of Info * Term
| TmPred of Info * Term
| TmIsZero of Info * Term


let rec isNumericalValue t = 
    match t with
    | TmZero(_) -> true
    | TmSucc(_, t1) -> isNumericalValue t1
    | _ -> false

let isValue t =
    match t with 
    | TmTrue(_) | TmFalse(_) -> true
    | _ -> isNumericalValue t


(*
    Evaluations:

    if true then t2 else t3 -> t2   (E-IfTrue)

    if false then t2 else t3 -> t3  (E-IfFalse)

    t1 -> t1'
    -----------------------------   (E-If)
    if t1 then t2 else t3 ->
    if t1' then t2 else t3

    t1 -> t1'
    -----------------------------   (E-Succ)
    succ t1 -> succ t1'

    pred 0 -> 0                     (E-PredZero)

    pred (succ nv1) ->  nv1         (E-PredSucc)

    t1 -> t1'
    -----------------------------   (E-Pred)
    pred t1 -> pred t1'

    iszero 0 -> true                (E-IsZeroZero)

    iszero (succ nv) -> false       (E-IsZeroSucc)

    t1 -> t1'
    -----------------------------   (E-IsZero)
    iszero t1 -> iszero t1'
*)

// exception to be raised when no evaluation rule applies
exception NoRuleApplies

// small-step evaluation
let rec smallStepEvaluate t =
    match t with
    | TmIf(_, TmTrue(_), t2, _) -> t2        // E-IfTrue
    | TmIf(_, TmFalse(_), _, t3) -> t3       // E-IfFalse
    | TmIf(fi, t1, t2, t3) ->                // E-If
        let t1' = smallStepEvaluate t1
        TmIf(fi, t1', t2, t3)
    | TmSucc(fi, t1) ->                      // E-Succ
        let t1' = smallStepEvaluate t1
        TmSucc(fi, t1')
    | TmPred(fi, TmZero(_)) -> TmZero(fi)    // E-PredZero
    | TmPred(_, TmSucc(_, nv1))              // E-PredSucc
        when isNumericalValue nv1 -> nv1
    | TmPred(fi, t1) ->                      // E-Pred
        let t1' = smallStepEvaluate t1
        TmPred(fi, t1')
    | TmIsZero(fi, TmZero(_)) -> TmTrue(fi)  // E-IsZeroZero
    | TmIsZero(fi, TmSucc(_)) -> TmFalse(fi) // E-IsZeroSucc
    | TmIsZero(fi, t1) ->                    // E-IsZero
        let t1' = smallStepEvaluate t1
        TmIsZero(fi, t1')
    | _ -> raise NoRuleApplies


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
