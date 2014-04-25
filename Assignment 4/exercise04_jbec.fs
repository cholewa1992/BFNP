//Exercise 4.1
//The question is not very undertandable... 
//Exercise 4.2
let rec IteretiveSum m n = if n > 0 then (m+n) + IteretiveSum m (n - 1) else m

//Exercise 4.3
let length list = 
    let rec lengthA list n = 
        if list = [] 
        then n 
        else 
            match list with 
            x::xs -> lengthA xs (n + 1)
    lengthA list 0;;

//Exercise 4.4
let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;

let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1, n*m);;

let id = (fun a -> a);;
let rec factC n c = if n = 0 then c 1 else factC (n-1) (fun a -> c (a * n));; 

let xs16 = List.init 17000000 (fun i -> 16);;

for i in xs16 do let _ = fact i in ();;         //Runtime 00:00:03.000
for i in xs16 do let _ = factA (i,1) in ();;    //Runtime 00:00:01.078
for i in xs16 do let _ = factC i id in ();;     //Runtime 00:00:10.203


//Exercise 4.5
let fib n =
    let mutable i = n
    let mutable x = 1
    let mutable y = 1

    while (i > 1) do 
        let oldy = y;
        x <- x + y
        y <- x - y
        i <- i - 1
    x;;


//Exercise 4.6
let rec fibA n x y = if n > 1 then fibA (n-1) (x+y) (x) else x
let rec fibC n c = if n > 1 then fibC (n-1) (fun x -> fibC (n-2) ( fun a  -> c a + x) )  else c 1
let rec fibD n c = if n > 1 then fibD (n-1) (fun x y -> c (x + y) x) else c 1 1

for i in xs16 do let _ = fib i in ();;                  //Runtime 00:00:01.296
for i in xs16 do let _ = fibA i 1 1 in ();;               //Runtime 00:00:00.453
for i in xs16 do let _ = fibC i id in ();;              // +120 secs
for i in xs16 do let _ = fibD i (fun x y -> x) in ();;  //00:00:13.194


//Exercise 4.7
type BinTree<'a> = Leaf | Node of BinTree<'a> * 'a * BinTree<'a>

let count tree = 
    let rec countA tree n = match tree with
        |Leaf -> n
        |Node(t1,l,t2) -> countA t1 (countA t2 (n + 1))
    countA tree 0

let tree = Node(Leaf, 1, Leaf);
let tree2 = Node(Node(Leaf,1,Leaf),2,Node(Node(Leaf,3,Leaf),4,Leaf))
let tree3 = Leaf
let tree4 = Node(Leaf,1,Node(Leaf,2,Node(Node(Leaf,3,Leaf),4,Leaf)))

let res1 = count tree       // val it : int = 1
let res2 = count tree2      //val it : int = 4
let res3 = count tree3      //vat it : int = 0
let res4 = count tree4      //val it : int = 4

//Exercise 4.8
let rec countAC tree a c = match tree with 
    |Leaf -> c a
    |Node(t1,n,t2) -> countAC t1 (a + 1) (fun d -> (countAC t2  d (fun d -> c d)))

//Exercise 4.9
//The problem is that (fun res -> 1::k(res)) is not tail-recursive.
//when making the fun tail-resursive it will not stack overflow

let rec bigListK n k =
    if n = 0 then k []
    else bigListK (n-1) (fun res -> k(1::res));;
 let res = bigListK 130000 id;;

//Exercise 4.10
let leftTree n = 
    let rec leftTreeA n1 a =
        if n1 > n then a
        else leftTreeA (n1+1) (Node(a, n1, Leaf))
    leftTreeA 0 Leaf

let rightTree n = 
    let rec rightTreeA n1 a =
        if n1 > n then a
        else rightTreeA (n1+1)  (Node(Leaf, n1, a))
    rightTreeA 0 Leaf

//count with leftTree 100 million : Out of memory exception
//count with rightTree 10000 : count = 10001
//count with rightTree 100000 : Stack overflow
//countAC with rightTree 100000000 : Out of memory exception
//countAC with leftTree 100000000 : Out of memory exception

//Exercise 4.11
let oddNumbers = Seq.initInfinite (fun i -> i * 2 + 1)

//Exercise 4.12
let rec f n a = if n < 1 then a else f (n - 1) (a * n)
let factSeq = Seq.initInfinite (fun i -> f i 1);;
