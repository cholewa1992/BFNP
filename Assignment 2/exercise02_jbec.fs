//Assignment 2.1
let rec downTo n =
    if n > 0
    then n :: downTo (n-1)
    else [];;

let rec downTo2 n = match n with
| x when x > 0 -> x :: downTo2 (x-1)
| _ -> [];;

//Assignment 2.2
let rec removeOddIdx = function
| x::y::xs -> x :: removeOddIdx xs
| x :: xs -> x :: removeOddIdx xs
| _ -> [];;

//Assignment 2.3
let rec combinePair = function
| x::y::xs -> (x,y) :: combinePair xs
| _ -> [] : list<int*int>;;

//Assignment 2.4
type bc = {Pound:int;Shilling:int;Pence:int}

let fromBc bc = (bc.Pound * 12 + bc.Shilling) * 20 + bc.Pence;;
let toBc x = {Pound = x / (12 * 20); Shilling = (x % (12 * 20)) / 20; Pence = (x % (12 * 20)) % 20};;

let (.+.) a b = toBc(fromBc a + fromBc b);;
let (.-.) a b = toBc(fromBc a - fromBc b);;


//Assignment 2.5
let (.+.) (a,b) (c,d) = (a + c, b + d) : (float * float);;
let (.*.) (a,b) (c,d) = (a*c - b*d, b*c - a*d) : (float * float);;
let (.-.) (a,b) = (-a,-b) : (float * float);;
let (.//.) a b = (a/(a**2.0 + b**2.0), -b/(a**2.0 + b**2.0));;
let (./.) (a,b) (c,d) = (a,b) .*. (c .//. d) ;;


//Assignment 2.6
let rec altsum = function
| [] -> 0
| x::xs -> x + (altsum xs * -1)

//Assignment 2.7
let explode s = List.ofArray((s : string).ToCharArray());;

let rec explode2 = function
| s when s = "" -> []
| s ->  (s : string).Chars(0) :: explode2 (s.Remove(0,1));;

//Assignment 2.8
let implode list = List.foldBack (fun x xs -> (x : char).ToString() + xs) list "";;
let implodeRev list = List.fold (fun xs x -> (x : char).ToString() + xs) "" (list : list<char>);;

//Assignment 2.9
let toUpper s = (s:string).ToUpper();;
let toUpper1 s = (List.foldBack (fun x xs -> System.Char.ToUpper(x).ToString() + xs) << explode2) s "";;
let toUpper2 s = "" |> s |> (explode >> List.foldBack(fun x xs -> System.Char.ToUpper(x).ToString() + xs))

//Assignment 2.10
let palindrome s = (toUpper >> explode >> implode) s = (toUpper >> explode >> implodeRev) s;;

//Assignment 2.11
let rec A = function
| (m,n) when m = 0 -> n+1
| (m,n) when m > 0 && n = 0 -> A(m-1,1)
| (m,n) when m > 0 && n > 0 -> A(m - 1, A(m, n-1));;

//The result of A(3,11) is 16381

//Assingment 2.12
let time f = 
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, (finish - start));;


let timeArgs f a = time (fun () -> f a);;

//Assignment 2.13
let rec downto1 = function
| (f,n,e) when n > 0 -> downto1(f,(n-1),e) |> f(n)
| (f,n,e) when n <= 0 -> e

let fact n = downto1 ((fun n e -> n * e), n, 1);;
let funlist g n = downto1 ((fun n e -> (g n)::e), n, []);;

//Example use of funlist: This function will make a list [n; n-1...;1]
let nTo1List n = funlist (fun n -> n) n;;