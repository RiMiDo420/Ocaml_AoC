let file = "day8_in.txt"

type dsuNode = Root | Link of int

let rec get_parent n dsu = 
  match dsu.(n) with
  |Root -> n
  |Link p -> let r = get_parent p dsu in
            Array.set dsu n (Link r);
            r

let rec join x y dsu =
  let a = get_parent x dsu in
  let b = get_parent y dsu in
  if a<>b then Array.set dsu a (Link b)

let rec join_bool x y dsu =
  let a = get_parent x dsu in
  let b = get_parent y dsu in
  if a<>b then (Array.set dsu a (Link b); true)
  else false


let add_num = List.mapi (fun x a -> (x, a)) 

let combine  (x1, a1) (x2, a2) = ((x1, x2), (a1, a2))
let make_all_pairs l1 l2= 
 List.map (fun x -> List.map (combine x) l2) l1 |> List.concat
let gen_combinations points = 
  make_all_pairs points points |>
  List.filter (fun ((x1, x2), a) -> x1<x2)

let getElem a b = (a-b)*(a-b)
let get_dist (ax, ay, az) (bx, by, bz) =
  sqrt(float_of_int(getElem ax bx + getElem ay by + getElem az bz)) 

let get_dists = 
  List.map(fun (x, (a1, a2)) -> (get_dist a1 a2, x))

let point_comp (d1, x1) (d2, x2) = Float.compare d1 d2

let get_counts l =
  let rec count_elems x count = function
    |[] -> count::[]
    |h::t when x=h -> count_elems x (count+1) t
    |h::t -> count::(count_elems h 1 t) in
  let li = List.sort compare l in
  count_elems (List.hd li) 0 li

let print_pair2 (a, b) = 
  print_int(a);
  print_char(',');
  print_int(b);
  print_newline()
let p1 (points:(int*int*int)list) = 
  let dsu = Array.init (List.length points) (fun x-> Root) in
  let edges = add_num points |>
  gen_combinations |>
  get_dists |>
  List.sort point_comp |>
  List.map (fun (d, x) -> x) |>
  List.take 1000 in
  List.iter print_pair2 edges;
  List.iter (fun (a, b) -> join a b dsu) edges;
  List.map (fun x -> get_parent x dsu) (List.init (List.length points) (fun x->x)) |>
  get_counts |>
  List.sort compare |>
  List.rev |>
  List.take 3 |>
  List.fold_left ( * ) 1


let rec last = function
|[] -> failwith("Empty list")
|h::[] -> h
|h::t -> last t

let calculate_p2 points (a, b)  =
  let (ax, ay, az) = List.nth points a in
  let (bx, by, bz) = List.nth points b in
  ax*bx

let p2 (points:(int*int*int)list) = 
  let dsu = Array.init (List.length points) (fun x-> Root) in
  add_num points |>
  gen_combinations |>
  get_dists |>
  List.sort point_comp |>
  List.map (fun (d, x) -> x) |>
  List.filter (fun (a, b) -> join_bool a b dsu)|>
  last |>
  calculate_p2 points


let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let get_pair_from_list l=
  if List.length l= 3 then
    (List.nth l 0, List.nth l 1, List.nth l 2)
  else failwith("invalid list length")

let processString s = 
  String.split_on_char ',' s |>
  List.map int_of_string |>
  get_pair_from_list

let get_input f = 
  read_file f |> List.map processString 


let print_pair (a, b, c) = 
  print_int(a);
  print_char(',');
  print_int(b);
  print_char(',');
  print_int(c);
  print_newline()

let print_input l = List.iter (fun x -> print_pair x) l

let main () =
  let input = get_input file 
  in
  print_input input;
  print_newline();
  print_int(p2 input);
  print_newline();;

main()






