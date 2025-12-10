let file = "day4_in.txt"

let getUp x = List.map (fun l-> 0::l |> List.take (List.length l)) x
let getDown x = List.map (fun l -> (List.tl  l) @ [0]) x

let rec genEmpty = function
|0 -> []
|x -> 0:: genEmpty (x-1)

let getLeft = function
|[] -> []
|h::t -> (genEmpty (List.length h))::(List.take (List.length (t)) (h::t))

let getRight = function
|[] -> []
|h::t ->  t @ [(genEmpty (List.length h))]

let printGrid  =
  List.map(fun x -> List.map print_int x; print_newline())

let printGrids  =
  List.map(fun x -> printGrid x; print_newline())

let addGrid a b = List.map2(fun c d -> List.map2(fun x y -> x+y) c d) a b

let andGrid a b = List.map2(fun c d -> List.map2(fun x y -> if x=1 then y else 0) c d) a b


let gridSum = function
|[] -> []
|h::t -> let g = List.fold_left addGrid h t in g

let getNeighborSum grid = 
let grids =[
  getUp grid;
  getDown grid;
  getLeft grid;
  getRight grid;
  getUp grid |> getLeft;
  getUp grid |> getRight;
  getDown grid |> getLeft;
  getDown grid |> getRight;
] in gridSum grids

let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let processString s = String.fold_right(fun c acc-> (if c='@' then 1 else 0)::acc) s []

let p1 grid = getNeighborSum grid |> 
List.map2 (fun l1 l2 -> List.map2 (fun isRoll neighbors -> if isRoll=1 then neighbors else 10) l1 l2) grid |>
List.map (fun l -> List.filter (fun x -> x< 4) l|> List.length) |> 
List.fold_left (fun acc x -> acc+x) 0 

let p1_deb grid = let g =  getNeighborSum grid |> 
List.map2 (fun l1 l2 -> List.map2 (fun isRoll neighbors -> if isRoll=1 then neighbors else 10) l1 l2) grid |>
List.map (fun l -> List.map (fun x -> if x< 4 then 1 else 0) l) in printGrid g;
List.map (fun l -> List.filter (fun x -> x= 1) l|> List.length) g |>
List.fold_left (fun acc x -> acc+x) 0

let rec p2 grid = let g =  getNeighborSum grid |> 
List.map2 (fun l1 l2 -> List.map2 (fun isRoll neighbors -> if isRoll=1 then neighbors else 10) l1 l2) grid |>
List.map (fun l -> List.map (fun x -> if x< 4 then 0 else 1) l) in printGrid g;
let removed = List.map (fun l -> List.filter (fun x -> x= 0) l|> List.length) g |>
List.fold_left (fun acc x -> acc+x) 0 in
if removed = 0 then 0
else removed + p2 (andGrid g grid)

let getInput f = 
  read_file f |> List.map processString 

let main () =
  let input = getInput file 
  in
  printGrid input;
  print_newline();
  print_int(p2 input);
  print_newline();;

main()
