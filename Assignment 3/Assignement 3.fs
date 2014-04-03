

type 'a BinTree = 
    |Leaf
    |Node of 'a * 'a BinTree * 'a BinTree;;

let intBinTree = Node(43, Node(25,Node (56, Leaf, Leaf), Leaf), Node(562, Leaf, Node(78, Leaf, Leaf)))


//Exercise 3.1
let rec inOrder tree = match tree with
    | Leaf -> []
    | Node(x,y,z) ->  inOrder y @ x :: inOrder z;

//Exercise 3.2
let rec mapInOrder f tree = match tree with
    | Leaf -> Leaf
    | Node(x,y,z) -> Node(f x, mapInOrder f y, mapInOrder f z)

//Exercise 3.3
let rec foldInOrder f n tree = match tree with
    | Leaf -> n
    | Node(x,y,z) -> foldInOrder f (f (foldInOrder f n y) x) z

//Exercise 3.4
type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *)

type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp (* conjunction *)

type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)

let rec I stm s =
match stm with
| Ass(x,a) -> update x (A a s)
| Skip -> s
| Seq(stm1, stm2) -> stm1 << stm2
                     //stm2
| ITE(b,stm1,stm2) -> if b = TT then stm1 else stm2
| While(b, stm) -> if b = TT then Seq(stm,While(b,stm))
                             else Skip;;

//Exercise 3.5
//Exercise 3.6
//Exercise 3.7
type Fexpr = 
| Const of float
| X
| Add of Fexpr * Fexpr
| Sub of Fexpr * Fexpr
| Mul of Fexpr * Fexpr
| Div of Fexpr * Fexpr
| Sin of Fexpr
| Cos of Fexpr
| Log of Fexpr
| Exp of Fexpr;;


let rec toPostfix stm = match stm with
| X -> "X"
| Const(x) -> x.ToString()
| Add(x,y) -> toPostfix x + toPostfix y + "+"
| Sub(x,y) -> toPostfix x + toPostfix y + "-"
| Mul(x,y) -> toPostfix x + toPostfix y + "*"
| Div(x,y) -> toPostfix x + toPostfix y + "/"
| Sin(x) -> toPostfix x + "sin"
| Cos(x) -> toPostfix x + "cos"
| Log(x) -> toPostfix x + "log"
| Exp(x) -> toPostfix x + "exp"


//Exercise 3.8
type Instruction = | ADD | SUB | MUL | DIV | SIN 
                   | COS | LOG | EXP | PUSH of float

type 'a Stack = VAL of 'a * 'a Stack | NONE

exception EmptyStack

let binaryOperation stack f = match stack with 
    | NONE -> raise EmptyStack
    | VAL(_,NONE) -> raise EmptyStack
    | VAL(x,VAL(y,s)) -> VAL(f y x, s);;

let unaryOperation stack f = match stack with 
    | NONE -> raise EmptyStack
    | VAL(x,s) -> VAL(f x, s);;

let intpInstr stack instruction = match instruction with 
| ADD -> binaryOperation stack (fun x y -> x + y)
| SUB -> binaryOperation stack (fun x y -> x - y)
| MUL -> binaryOperation stack (fun x y -> x * y)
| DIV -> binaryOperation stack (fun x y -> x / y)
| SIN -> unaryOperation stack (fun x -> sin x)
| COS -> unaryOperation stack (fun x -> cos x)
| LOG -> unaryOperation stack (fun x -> log x)
| EXP -> unaryOperation stack (fun x -> exp x)
| PUSH(x) -> VAL(x,stack);;

let rec intpListOfInstr stack list = match list with
| [] -> match stack with VAL(x,s) -> x
| x :: xs -> intpListOfInstr (intpInstr stack x) xs
             
let rec trans stm value = match stm with
| X -> [PUSH value]
| Const(x) -> [PUSH x]
| Add(x,y) -> trans x value @ trans y value @ [ADD]
| Sub(x,y) -> trans x value @ trans y value @ [SUB]
| Mul(x,y) -> trans x value @ trans y value @ [MUL]
| Div(x,y) -> trans x value @ trans y value @ [DIV]
| Sin(x) -> trans x value @ [SIN]
| Cos(x) -> trans x value @ [COS]
| Log(x) -> trans x value @ [LOG]
| Exp(x) -> trans x value @ [EXP];;

//Exercise 3.9
