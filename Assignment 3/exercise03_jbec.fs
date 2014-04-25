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

//Exercise 3.4, 3.5 & 3.6
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
    | IT of bExp * stm
    | RU of stm * bExp
    | Inc of string
 
 
 
let update x v s = Map.add x v s : Map<string,int>
 
let rec A a s = match a with
    | N(x) -> x
    | V(x) -> Map.find x s
    | Add(x,y) -> (A x s) + (A y s)
    | Mul(x,y) -> (A x s) * (A y s)
    | Sub(x,y) -> (A x s) - (A y s)
 
type state = Map<string,int>
 
let rec B b s = match b with
    | TT -> true
    | FF -> false
    | Eq(x,y) -> (A x s) = (A y s)
    | Lt(x,y) -> (A x s) < (A y s)
    | Neg(x) ->  not (B x s)
    | Con(x,y) -> B x s && B y s
 
let rec I stm s =
    match stm with
    | Ass(x,a) -> update x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> let u = I stm1 s in I stm2 u
    | ITE(b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b, stm1) -> if B b s then I stm (I stm1 s) else s
    | IT(b, stm1) -> if B b s then I stm1 s else s
    | RU(stm1, b) -> I (While(Neg(b), stm1)) s
    | Inc(x) -> I (Ass(x, Add(V x, N 1))) s
   
//Default state
let s0 = Map.ofList [("x", 4)];;

//Example runs
let ex1 = Seq( Ass ( "y", N 1), While(Neg(Eq(V "x", N 0)), Seq ( Ass ( "y", Mul (V "x", V "y")) , Ass ("x", Sub( V "x", N 1)))))
let ex2 = Seq( Ass ( "y", N 1), While(Neg(Eq(V "x", N 4)), Seq ( Ass ( "y", Mul (V "x", V "y")) , Inc("x"))))
let ex3 = RU(Inc("x"), Eq(V "x", N 100))
let ex4 = IT(Eq(V "x", N 10), Inc("x"))
let ex5 = ITE(Lt(V "x", N 0), Ass ("y", N 0), Ass ("y", N 1))

let rex1 = I ex1 s0;
let rex2 = I ex2 s0;
let rex3 = I ex3 s0;
let rex4 = I ex4 s0;
let rex5 = I ex5 s0;


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

type 'a Stack = VAL of 'a * 'a Stack | EMPTY

exception EmptyStack

let binaryOperation stack f = match stack with 
    | EMPTY -> raise EmptyStack
    | VAL(_,EMPTY) -> raise EmptyStack
    | VAL(x,VAL(y,s)) -> VAL(f y x, s);;

let unaryOperation stack f = match stack with 
    | EMPTY -> raise EmptyStack
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
(*  Please see attached ComplexNumbers.fs and ComplexNumbers.fsi *)