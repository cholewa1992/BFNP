//Exercise 1.1
let sqr x = x*x;;

//Exercise 1.2
let pow x n = System.Math.Pow(x,n);; //float -> float -> float

let rec pow2 = function
| (_,0.0) -> 1.0
| (x,n) -> x * pow2(x,n - 1.0);;
//float * float -> float

//Exercise 1.3
let g n = n + 4;;

//Exercise 1.4
let h (x,y) = System.Math.Sqrt(x*x + y*y);; // float * float -> float

//Exercise 1.5
let rec f = function
| 0 -> 0
| n -> n + f(n-1);;

//  f 4
//  4 + f(4 - 1)
//  4 + (3 + f(3 - 1))
//  4 + (3 + (2 + f(2 - 1)))
//  4 + (3 + (2 + (1 + f 0)))
//  4 + (3 + (2 + (1 + 0)))
//  4 + (3 + (2 + 1))
//  4 + (3 + 3)
//  4 + 6
//  10

//Exercise 1.6
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib(n - 1) + fib(n - 2);;

//Exercise 1.7
let rec sum = function
| (m,0) -> m
| (m,n) -> (m + n) + sum(m, n - 1);;

//Exercise 1.8

    //  (System.Math.PI, fact -1);; float * int
    //  fact(fact 4);;  int
    //  power(System.Math.PI, fact 2);; float
    //  (power, fact) (float * int -> float) * (int -> int)

//Exercise 1.9

    //  ([a -> 5])
    //  (a, a + 1, [])
    //  (b, f(b) + a, [f a -> a + 1, a -> 5])


//Exercise 1.10
let dup s = s^s;;

//Exercise 1.11
let rec dubn = function
| (x,1) -> x
| (x,n) -> x^dubn(x, n-1);;

//Exercise 1.12
let timediff (h2,m2) (h1,m1) = (h2-h1) * 60 + m2 - m1

//Exercise 1.13
let minutes (h,m) = timediff (h,m) (0,0);;

//Exercise 1.14
let rec dubn2 = function
| (x,1) -> x
| (x,n) -> x^dubn2(x, n-1);;

//Exercise 1.15
let rec bin = function
| (n,k) when n <= k || k <= 0 -> 1
| (n,k) -> bin (n-1, k-1) + bin (n-1, k);;

//Exercise 1.16
(*
    1. Determine the type of f
    The type of f is int * int -> int

    2. For which arguments does the evaluation of f terminate?
        For all arguments where x is an positiv integer (int.max => x >= 0)

    3. Write the evaluation steps for f(2,3)
        1. (2,3) -> f(2-1, 2*3)
        2. (1,6) -> f(1-1,6*1)
        3. (0,6) -> 6

    4. What is the mathematical meaning of f(x,y)?
        f(x,y) = y*(x!)

//Exercise 1.17
    1. What is the type of test
        bool * int -> int

    2. What is the result of evaluating test(false, fact(-1))
        It will not terminate due to the definition of fact.

    3. Compare this with the result of evaluating if false then fact -1 else 0
        This will be 0. In this case the program will not try to evaluate fact -1, 
        therefore the program will not stack overflow like the previous evaluation.
*)

//Exercise 1.18
let curry f x y = f (x,y);; // ('a * 'b -> 'c) -> 'a -> 'b -> 'c
let uncurry g (x,y) = g x y;; // ('a -> 'b -> 'c) -> 'a * 'b -> 'c




