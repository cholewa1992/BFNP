type Name = string;;
type Score = int;;
type Result = Name * Score;;

//1.1
let legalResults list = List.forall (fun (n,x) -> x > 0 && x < 100) list

//1.2
let maxScore list = 
    let rec maxScoreRec list h  = match list with 
        |[] -> h
        |(n,x)::xs -> maxScoreRec xs (if x > h then x else h)
    maxScoreRec list 0;;

//1.3
let rec best list =
    let rec bestrec list (n,s) = match list with
        |[] -> (n,s)
        |(x,y)::xs -> bestrec xs (if y > s then (x,y) else (n,s))
    bestrec list ("",0);;

let rec best2 list =
    let max = maxScore list
    List.find (fun (n,s) -> s = max) list


//1.4
let average list =
    let rec length list n = match list with
        |[] -> n
        | _::xs -> length xs (n + 1)

    let rec sumrec list n = match list with
        |[] -> n
        |(x,y)::xs -> sumrec xs (n + y)
    (sumrec list 0) / (length list 0);;

//1.5
let rec delete r list = match list with
    | [] -> list
    | x::xs -> (if x = r then delete r xs else x::(delete r xs))

//1.6
let bestN list n = 
    if(n > List.length list) then failwith "n was greater than the length of the list"
    
    let rec take list c = match list with 
        | [] -> list
        | x::xs -> if c < n then x::(take xs (c+1)) else take [] 0
    take (List.sortBy (fun (n,s) -> s * -1) list) 0
    

//2.1
type Typ =  |Integer
            |Boolean
            |Ft of Typ list * Typ
type Decl = string * Typ

let rec distinctVars list = match list with
        |[] -> true
        |(s,t)::xs -> (not (List.exists (fun (x,y) -> s = x) xs)) && distinctVars xs
    

//2.2
type SymbolTable = Map<string,Typ>;;

let rec toSymbolTable list = match list with
    |[] -> Map.empty
    |x::xs -> (toSymbolTable xs).Add x

//2.3
let rec extendST list (st : SymbolTable) = match list with
    |[] -> st
    |(s,t)::xs -> if st.ContainsKey s then (st.Remove s).Add (s,t) else st.Add (s,t)

//2.4
type Exp =  | V of string
            | A of string * Exp list


let rec symbolsDefined (st : SymbolTable) e = match e with
    | V(s) -> st.ContainsKey s
    | A(s,e) -> List.fold (fun xs x -> xs && (symbolsDefined st x)) (st.ContainsKey s) e

//2.5

let rec getExpressionTypeList (st : SymbolTable) e = match e with
    | V(s) -> [st.Item s]
    | A(s,e) -> List.fold (fun xs x -> getExpressionTypeList st x @ xs ) [] e

let rec getTypeList (st : SymbolTable) t = match t with
    | Ft(l,t) -> List.fold (fun xs x -> getTypeList st x @ xs) [] l
    | _ -> [t]

let wellTypedExp (st : SymbolTable) e = match e with
    | A(s,l) -> (getExpressionTypeList st e) = (getTypeList st (st.Item s))
    | V(s) -> true

let getType (st : SymbolTable) t = match t with
    | Ft(l,t) -> t
    | _ -> t

let typOf (st : SymbolTable) e =
    if not (wellTypedExp st e) then failwith "Not well typed"
    match e with 
    | V(s) -> getType st <| st.Item s
    | A(s,e) -> getType st <| st.Item s

//2.6
type Stm =  | Ass of string * Exp
            | Seq of Stm * Stm
            | Ite of Exp * Stm * Stm
            | While of Exp * Stm
            | Block of Decl list * Stm

let rec wellTyped (st : SymbolTable) (stm : Stm) = match stm with
    | Ass(s,e) -> (getType st <| st.Item s) = typOf st e
    | Seq(s1,s2) -> wellTyped st s1 && wellTyped st s2
    | Ite(e,s1,s2) -> typOf st e = Boolean && wellTyped st s1 && wellTyped st s2
    | While(e,s) -> typOf st e = Boolean && wellTyped st s
    | Block(l,s) -> distinctVars l && wellTyped (extendST l st) s