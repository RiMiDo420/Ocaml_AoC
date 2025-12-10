let file = "day7_in.txt"
let gen_next current splitters = 
 let rec gen_next_beams current splitters acc= 
    match current, splitters with
    |[], [] -> List.rev acc
    |h::tc, false::ts -> gen_next_beams tc ts (h::acc)
    |h1::h2::tc, true::ts -> gen_next_beams ((h1+h2)::tc) ts (0::h1+(List.hd acc)::(List.tl acc))
    |h::[], true::ts -> gen_next_beams [] ts (0::h+(List.hd acc)::(List.tl acc))
    |_, _ -> failwith("Lists are not the same length")
in
gen_next_beams current splitters []

let rec get_next_splits current splitters = 
  match current, splitters with
  |[], [] -> 0
  |h::tc, true::ts when h>0-> 1+ get_next_splits tc ts
  |h1::t1, h2::t2 ->  get_next_splits t1 t2
  |_, _ -> failwith("Lists are not the same length")

let rec generateSplits start = function
|[] -> []
|h::t -> start::(generateSplits (gen_next start h) t)

let rec toIntList = function
|[] -> []
|false::t -> 0::(toIntList t)
|true::t -> 1::(toIntList t)

let rec last = function
|[] -> failwith("List is empty")
|h::[] -> h
|h::t -> last t

let p1 grid = 
  let start = List.hd grid |> toIntList in
  let splitters = List.tl grid in
  generateSplits start splitters |>
  List.fold_left2 (fun acc a b -> acc + (get_next_splits b a)) 0 splitters 

let p2 grid = 
  let start = List.hd grid |> toIntList in
  let splitters = List.tl grid in
  generateSplits start splitters |>
  last |>
  List.fold_left (+) 0



let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let processString s = String.fold_right(fun c acc-> (if c='^'||c='S' then true else false)::acc) s []

let get_input f = 
  read_file f |> List.map processString 

let printGrid  =
  List.map(fun x -> List.map (fun a -> if a then print_int 1 else print_int 0) x; print_newline())

let main () =
  let input = get_input file 
  in
  print_newline();
  print_int(p2 input);
  print_newline();;

main()