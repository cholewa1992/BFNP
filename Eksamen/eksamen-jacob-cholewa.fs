//I, Jacob Benjamin Cholewa, hereby declare that I myself have created this exam hand-in in it's entirety without help from anybody else

//Question 1.1
type OrderedList<'a when 'a : equality> = { front: 'a list; rear: 'a list}
let ol1 = {front = ["Hans"; "Brian"; "Gudrun";]; rear = []}
let ol2 = {front = ["Hans"; "Brian";]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"; ]; rear = ["Gudrun"; "Brian";]}
let ol4 = {front = []; rear = ["Gudrun"; "Brian";"Hans";]}
//There exists four representations all shown above

//Question 1.2
//This function takes an Orderedlist. If the rear list is non empty then front and the revered rear list is concatenated and a new ordered list is returned.
let canonical list = if list.rear = [] then list  else { front = (list.front @ List.rev list.rear); rear = []}

//This function takes an OrderedList, makes a canonical representation and returned the front list now contaning all elements.
let toList list = (canonical list).front

//Question 1.3
let newOL = {front = []; rear = []} //This function returns a new empty OrderedList
let isEmpty list = list.front = [] && list.rear = [] //This function takes an OrderedList and asserts that both front and rear list is empty

//Question 1.4
//This function adds an element to the front of the OrderedList. This means to the frontlist in the OrderedList
let addFront element list = {front = element::list.front; rear = list.rear} 

//This function removes the first element in the OrderedList. If the list is empty an exception is thrown. 
//If the front list is empty the list is made canonical before the first element in the front list is removed
let rec removeFront list = match list with
| {front = []; rear = []} -> failwith "Ordered list is empty" 
| {front = []; rear = ys} -> removeFront << canonical <| list //<< means that the result from canonical is given to removeFront as parameter. <| is just passing a parameter to the joined function
| {front = x::xs; rear = ys} -> {front = xs; rear = ys}


//This function peeks the first element in the OrderedList. If the list is empty an exception is thrown. 
//If the front list is empty the list is made canoical before the first element in the front list is returned
let rec peekFront list = match list with
| {front = []; rear = []} -> failwith "Ordered list is empty" 
| {front = []; rear = ys} -> peekFront << canonical <| list
| {front = x::xs; rear = ys} -> x

//Question 1.5
//This functions canonicalate the two lists and append their front lists.
let append list1 list2 ={front = (canonical list1).front @ (canonical list2).front; rear = []}

//Question 1.6
//This function maps a list to a new list with a given function. The function uses List.map on both the front and rear list to adapt a similar behavior
let map f list = match list with
| {front = []; rear = []} -> newOL
| {front = xs; rear = ys} -> { front = List.map f xs; rear = List.map f ys}

//Question 1.7
//This function folds an OrderedList. First the list is made canoical then List.fold is applied to the front list.
let rec fold f a list = match list with 
| {front = xs; rear = []} -> List.fold f a xs
| {front = _; rear = ys} -> fold f a << canonical <| list

//This function takes an OrderedList and makes in into a regualar list before appying List.fold.
let rec fold2 f a list = List.fold f a << toList <| list

//Question 1.8
//This function uses fold to add or increment the times an element in the list has occured. 
//The given function checks if the element exsist. If it exsists it increments otherwise adds it with 1
let rec multiplicity list = fold (fun (acc : Map<'a,int>) x -> if acc.ContainsKey x then acc.Add(x,(acc.Item x + 1)) else acc.Add(x,1)) Map.empty list

//Question 2.1
let rec f i = function
| [] -> [i]
| x::xs -> i+x :: f (i+1) xs

//1: F x computes a function that given a list, fx with the list [i;j;k] with size n, returns a new list with elements [i+x+0;j+x+1;k+x+2;x+n]
//2: The result of f given any input will never return an empty list as the base case [] -> [i] will always return a list with atleast 1 element
//3: F cannot go into an infinit loop as it will empty out the given list, and when the list is empty terminate. 
//Therefore the number of recurvice loops is determined by the size of the list and a list can not be infinite

//Question 2.2
//This is an implementation of f using accumulation
let rec fA i a list = match list with
| [] -> a @ [i]
| x::xs -> fA (i+1) (a @ [i+x]) xs

//Question 2.3
//This is an implementation of f using continuation
let rec fC i c = function
| [] -> c [i]
| x::xs -> fC (i+1) (fun a -> c (i+x::a)) xs

//Question 3.1
let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]
//The function above returns a sequence with M+1 numbers in the sequence of n
//eg: myFinSeq 2 4 = seq [2;4;6;8;10]; hence the 5 first numbers in the sequence of 2
//eg: myFinSeq 3 4 = seq [3;6;9;12;16]; hence the 5 first numbers in the sequence of 3

//Question 3.2
//This function is an infinite sequence of n+n*i where i >= 0. 
//Hence, it's the same sequence as the one above except it's juat infinit instead of finite
let mySeq n = Seq.initInfinite (fun i -> n+n*i)

//Question 3.3
//This sequence returns multiplication tables for N*M in the type (N,M,N*M)
let multTable N M = seq { for i in 0..N do for j in 0..M do yield (i,j,i*j) }

//Question 3.4
//This sequence uses multTable to print the multiplication triplets
let ppMultTable N M = seq { for i in 0..(N*M) do yield let (x,y,z) = Seq.nth i (multTable N M) in x.ToString() + " * " + y.ToString() + " is " + z.ToString() }

//Question 4.1
type opr = 
    |MovePenUp
    |MovePenDown
    |TurnEast
    |TurnWest
    |TurnNorth
    |TurnSouth
    |Step
    
type plot = 
    |Opr of opr 
    | Seq of plot * plot 
    
//This function takes an operation and returns a meaningful string. This is done with pattern matching
let ppOpr opr = match opr with
    |MovePenUp -> "MovePenUp"
    |MovePenDown -> "MovePenDown"
    |TurnEast -> "TurnEast"
    |TurnWest -> "TurnWest"
    |TurnSouth -> "TurnSouth"
    |TurnNorth -> "TurnNorth"
    |Step -> "Step"

//This recurcive function takes a plot and aggragates a string using the ppOpr method.
let rec ppOprPlot plot = match plot with
    | Opr(opr) -> ppOpr opr
    | Seq(p1,p2) -> (ppOprPlot p1) + " -> " + (ppOprPlot p2)

//Question 4.2
type dir = North|West|South|East
type pen = PenUp|PenDown
type coord = int * int
type state = coord * dir * pen

//This function takes a step and returns a new step acoording to the direction given.
let goStep step = match step with
    | ((x,y),North,p) -> ((x,y+1),North,p)
    | ((x,y),West,p) -> ((x-1,y),West,p)
    | ((x,y),South,p) -> ((x,y-1),South,p)
    | ((x,y),East,p) -> ((x+1,y),East,p)

//This function takes a state and a list of coordianates and executes a given operation
let addDot (c,d,p) list opr = match opr with
    |MovePenUp -> (list,(c,d,PenUp))
    |MovePenDown -> (c::list,(c,d,PenDown))
    |TurnEast -> (list,(c,East,p))
    |TurnWest -> (list,(c,West,p))
    |TurnSouth -> (list,(c,South,p))
    |TurnNorth -> (list,(c,North,p))
    |Step -> match goStep (c,d,p) with (c,d,p) -> if p = PenDown then (c::list,(c,d,p)) else (list,(c,d,p))

let (coords1, s1) = addDot ((0,0),East,PenUp) [] MovePenDown
let (coords2, s2) = addDot s1 coords1 Step

//This function takes a plot and returns a set of coordinates. It used the addDot function defined before. 
let dotCoords plot = 
    let rec dotCoordsRec plot state coords = match plot with
        | Opr opr -> addDot state coords opr
        | Seq(s1,s2) -> match dotCoordsRec s1 state coords with (coords,state) -> dotCoordsRec s2 state coords
    match dotCoordsRec plot ((0,0), East, PenUp) [] with (coords,state) -> coords

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))
let rect = Seq(Seq(Opr TurnEast, side), Seq(Opr TurnNorth, Seq(side, Seq(Opr TurnWest, Seq(side, Seq(Opr TurnSouth, side))))))

//This function takes a plot and returns a set of coordinates. This funciton is build on dotCoords
let uniqueDotCoords plot =
    let rec loop coords = match coords with
        | [] -> Set.empty
        | x::xs -> Set.add x (loop xs)
    loop << dotCoords <| plot

//Question 4.3
//The plot method have been extended to have the operator + as Seq(p1,p2)
let (+) (p1:plot) (p2:plot) = Seq(p1,p2)

//So that notions like this is possible 
let side2 = Opr MovePenDown + Opr Step + Opr Step + Opr Step
let rect2 = Opr TurnEast + side2 + Opr TurnNorth + side2 + Opr TurnWest + side2 + Opr TurnSouth + side2