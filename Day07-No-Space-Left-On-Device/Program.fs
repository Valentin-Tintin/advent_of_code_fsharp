open Microsoft.FSharp.Core

type AocFile = {Name: string; Size: int}


type Tree = Leaf of AocFile | Section of string * Tree list 

// Breadcrumb
type Path = Top | Node of Tree list * (string * Path) * Tree list


type Location = Loc of Tree * Path



let testTree = Section("root", List.Empty)

let testZipper = Loc(testTree, Top)



let left (Loc(t, p)) =
    match p with
    | Top -> failwith "root cant move left"
    | Node(l :: left, up, right) -> Loc(l, Node(left, up, t :: right))
    | Node([], _, _) -> failwith "already outer left"
    
let right (Loc(t, p)) =
    match p with
    | Top -> failwith "root cant move right"
    | Node(left, up, r :: right) -> Loc(r, Node(t :: left, up, right))
    | Node(_, _, []) -> failwith "already outer right"
    
let rec outer_right (Loc(t, p)) =
    match p with
    | Top -> failwith "root cant move right"
    | Node(_, _, []) -> Loc(t,p)
    | Node _ -> outer_right (right (Loc(t,p)))
        
let up (Loc(t,p)) = 
    match p with
    | Top -> failwith "already on top"
    | Node (left, up, right) -> Loc(Section(fst up, (List.rev left) @ (t::right)), snd up)
    
    
let down (Loc(t, p)) = 
    match t with
    | Leaf _ -> failwith "lowest point"
    | Section(name,t1::trees) -> Loc(t1, Node([], (name,p), trees))
    | _ -> failwith "not a tree"
    
let rec root (Loc(t, p) as l) = 
    match p with 
    | Top -> t
    | _ -> root (up l)

let insert_right r (Loc(t, p)) = 
    match p with
    | Top -> failwith "insert at top"
    | Node(left, up, right) -> Loc(t, Node(left, up, r::right))

let insert_down t1 (Loc(t, p)) = 
    match t with
    | Leaf _ -> failwith "cant create subdir of file"
    | Section(name, dir) -> Loc (t1, Node([], (name,p), dir))
    

let rec search_dir_left dirName (Loc(t,p))=
    match p with
    | Top -> failwith "top"
    | Node(_, parent, _) when (match t with
                                | Section(name, _) when name = dirName -> true
                                | _ -> false) -> Loc(t,p)
    | Node([], _, _) -> failwith "outer left reached"
    | Node _ -> search_dir_left dirName (left (Loc(t,p)))
    | _ -> failwith "todo"

let down_to_dir dirName (Loc(t, p)) =
    match t with
        | Leaf _ -> failwith "lowest point"
        | Section(name,t1::trees) -> down (Loc(t,p)) |> outer_right |> search_dir_left dirName
        | _ -> failwith "todo"
    
   

let start = testZipper

let fileA = Leaf({Name= "fileA"; Size=12});
let fileB = Leaf({Name= "fileB"; Size=24});
let dirA = Section("dirA", []);
let dirB = Section("dirB", []);



let x = insert_down fileA testZipper |> insert_right dirA |> insert_right fileB |> insert_right dirB |> outer_right |> up |> down_to_dir "dirB"


let stop = true