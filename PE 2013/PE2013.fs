//1.1
type item = { 
                id : int; 
                name : string; 
                price :float
            }
type register = item list


let reg : register = ([
    {id =1; name = "Milk"; price = 8.75;};
    {id =2; name = "Juice"; price = 16.25;};
    {id =3; name = "Rye Bread"; price = 25.00;};
    {id =4; name = "White Bread"; price = 18.50;};]);

//1.2
exception Register of string
let rec getItemById id = function
| [] -> raise (Register "Element not found")
| x::xs -> if x.id = id then x else getItemById id xs

//1.3
let nextId reg =
    let rec nextIdrec reg h = match reg with
    | [] -> h + 1
    | x::xs -> nextIdrec xs (if x.id > h then x.id else h)
    nextIdrec reg 0;;

//1.4
let addItem name price reg = reg @ [{id = nextId reg; name = name; price = price;}]

//1.5
let rec deleteItemById id reg = match reg with
| [] -> reg
| x::xs -> if x.id = id then deleteItemById id xs else x::deleteItemById id xs

//1.6
let rec uniqueRegister reg = match reg with
| [] -> true
| x::xs -> not (List.exists (fun i -> x.id = i.id) xs) && uniqueRegister xs

//1.7
let rec itemsInPriceRange min max reg = match reg with
| [] -> reg
| x::xs -> if x.price >= min && x.price <= max then x::itemsInPriceRange min max xs else itemsInPriceRange min max xs

//2.1
let rec f n m = if m = 0 then n else n * f (n+1) (m-1)
//The type of f is int -> int -> int. It computes the series of  n+0 * n+1 * n+2 * ... n+m

//2.2
let rec f' n m a = if m = 0 then a else f' (n+1) (m-1) (a * (n+1))

//2.3
let rec z xs ys = match (xs,ys) with
| ([],[]) -> []
| (x::xs,[]) -> (x,x) :: (z xs ys)
| ([], y::ys) -> (y,y) :: (z xs ys)
| (x::xs,y::ys) -> (x,y)::(z xs ys)

//Its type is a' list -> a' list -> (a' * a') list. It compues a list of pairs made from xs an ys.
//Eg1: xs = [1;2;3] ys = [4;5;6]
//z xs ys = [(1,4);(2,5);(3,6);]

//Eg2: xs = [1] ys = [4;5;6]
//z xs ys = [(1,4);(5,5);(6,6);]

//2.4
let rec s xs ys = match (xs,ys) with
| ([],[]) -> []
| (xs,[]) -> xs
| ([],ys) -> ys
| (x::xs,y::ys) -> x::y::s xs ys

//The type is 'a list -> 'a list -> 'a list.
//It merges xs and ys into one list: xs = [x1;x2] ys = [y1;y2;y3] s xs ys = [x1;y1;x2;y2;y3]

//2.5
let  sc xs ys = 
    let rec scRec xs ys c = match (xs,ys) with
    | ([],[]) -> c []
    | (xs,[]) -> c xs
    | ([],ys) -> c ys
    | (x::xs,y::ys) -> scRec xs ys (fun a -> c <| x::y::a)
    scRec xs ys id;;

//3.1
type Latex<'a> =
    Section of string * 'a * Latex<'a>
    | Subsection of string * 'a * Latex<'a>
    | Text of string * Latex<'a>
    | Ref of string * Latex<'a>
    | Label of string * Latex<'a>
    | End

let text1 = Section ("Introduction", None,
    Text ("This is an introduction to ...",
        Subsection ("A subsection", None,
            Text ("As laid out in the introduction we ...",
                End))))


//The type is Latex<'a option>

//3.2
let text2 = Section ("Introduction", None,
    Text ("This is an introduction to ...",
        Subsection ("A subsection", None,
            Text ("As laid out in the introduction we ...",
                Subsection ("Yet a subsection", None,
                    Section ("And yet a section", None,
                        Subsection ("A subsection more...", None,
                             End)))))))

let addSecNumbers text =
    let rec addSecNumbersRec text sn ssn = match text with 
    | Section(s,a,l) ->  Section(s,(sn + 1).ToString(), addSecNumbersRec l (sn + 1) 0)
    | Subsection(s,a,l) ->  Subsection(s,sn.ToString() + "." + (ssn + 1).ToString(), addSecNumbersRec l sn (ssn + 1))
    | Text(s,l) -> Text(s, addSecNumbersRec l sn ssn)
    | Ref(s,l) -> Ref(s,addSecNumbersRec l sn ssn)
    | Label(s,l) -> Label(s,addSecNumbersRec l sn ssn)
    | End -> End
    addSecNumbersRec text 0 0

//3.3
//The addSecNumers has the type Latex<'a> -> Latex<string>

//3.4
let text3 = Section ("Introduction", "1",
    Label("intro.sec",
        Text ("In section",
            Ref ("subsec.sec",
                Text (" we describe ...",
                    Subsection ("A subsection", "1.1",
                        Label("subsec.sec",
                            Text ("As laid out in the introduction, Section ",
                                Ref ("intro.sec",
                                    Text (" we ...",
                                        End))))))))))

let buildLabelEnv text =
    let rec buildLabelEnvRec text = match text with
        | End -> Map.empty
        | Section(_,sn,Label(s,l)) -> (buildLabelEnvRec l).Add(s, sn)
        | Section(_,_,l) -> buildLabelEnvRec l
        | Subsection(_,ssn,Label(s,l)) -> (buildLabelEnvRec l).Add(s, ssn)
        | Subsection(_,_,l) -> buildLabelEnvRec l
        | Text(_,l) -> buildLabelEnvRec l
        | Ref(_,l) -> buildLabelEnvRec l
        | Label(_,l) -> buildLabelEnvRec l
    buildLabelEnvRec  (addSecNumbers text)

//3.5

let toString text =
    let be = buildLabelEnv text
    let nl = System.Environment.NewLine
    let rec toStringRec text = match text with
        | End -> ""
        | Section(s,sn,l) -> nl + sn + " " + s + nl + toStringRec l 
        | Subsection(s,ssn,l) -> nl + ssn + " " + s + nl + toStringRec l
        | Text(s,l) -> s + toStringRec l
        | Ref(s,l) -> be.Item s + toStringRec l
        | Label(s,l) -> toStringRec l
    toStringRec (addSecNumbers text)

//4.1
let mySeq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i)

let r = Seq.take 10 mySeq
//The type is Seq<int> and the result is [0;1;-2;3;-4;5;-6;7;-8;9]

//4.2
let finSeq n M = Seq.init M (fun i -> n+i*2)

//4.3
type X = A of int | B of int | C of int * int

let rec zX xs ys = match (xs,ys) with
|(A a::aS, B b::bS) -> C(a,b) :: zX aS bS
| ([],[]) -> []
| _ -> failwith "Error"

//Type A list -> B list -> C list
let zXeg1 = zX [A 1;A 2;A 3] [B 1; B 2;B 3] // = [C (1,1); C (2,2); C (3,3)]
let zXeg2 = zX [A 1;A 2;A 3] [B 4; B 5;B 6] // = [C (1,4); C (2,5); C (3,6)]
let zXeg3 = zX [A 3;A 2;A 1] [B 1; B 2;B 3] // = [C (3,1); C (2,2); C (1,3)]

let rec uzX xs = match xs with
| C(a,b)::cS -> let (aS,bS) = uzX cS in (A a::aS,B b::bS)
| [] -> ([],[])
| _ -> failwith "Error"

//Type C list -> A list * B list
let uzXeg1 = uzX [C (1,1); C (2,2); C (3,3)] // = ([A 1;A 2;A 3],[B 1; B 2;B 3])
let uzXeg2 = uzX [C (1,4); C (2,5); C (3,6)] // = ([A 1;A 2;A 3],[B 4; B 5;B 6])
let uzXeg3 = uzX [C (3,1); C (2,2); C (1,3)] // = ([A 3;A 2;A 1],[B 1; B 2;B 3])