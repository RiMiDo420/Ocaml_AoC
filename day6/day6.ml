let file = "day6_in.txt"
let rec addList l1 l2 = match l1, l2 with
|[], [] -> []
|h1::t1, h2::t2 -> (h2::h1)::(addList t1 t2)
|_, _ -> failwith("Lists have different lengths")

let rec genEmpty = function
|0 -> []
|x -> []::(genEmpty (x-1))
let transpose m =
  let empty = List.length (List.hd m )|> genEmpty in
  List.fold_left addList empty m

let calculateExample =function
|1::t -> List.fold_left (+) 0 t
|2::t -> List.fold_left ( * ) 1 t
|_ -> failwith("Invalid example")
let p1 input = 
  transpose input |>
  List.map calculateExample |>
  List.fold_left (+) 0

let p2 input = 
  List.map calculateExample input|>
  List.fold_left (+) 0

let printList l= List.map (fun x -> print_string x; print_char ' ') l; print_newline()

let printIntList l= List.map (fun x -> print_int x; print_char ' ') l; print_newline()

let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let getNum = function
|"+" -> 1
|"*" -> 2
|x -> int_of_string x

let process_input s = 
  String.split_on_char ' ' s |> 
  List.map String.trim |>
  List.filter (fun x -> x<>"") |>
  List.map getNum



let get_input f = read_file f |> List.map process_input

let rec group l= 
  let rec group_eq l acc = match l with
  |[] -> acc::[]
  |""::t -> acc::(group_eq t [])
  |h::t -> group_eq t ((int_of_string h)::acc)
in
  group_eq l []


let to_char_arr l c= c::l

let rec to_string chars n = match chars with
|[] -> failwith("char list not long enough")
|h::t when n=0 -> h
|h::t -> to_string t (n-1)
let getNumLists sm = 
  List.map (String.fold_left to_char_arr []) sm|>
  transpose |>
  List.map List.rev |>
  List.map (fun c -> String.init (List.length c) (to_string c)) |>
  List.map String.trim |>
  group



let process_input2 s =
  let nums = List.take ((List.length s)-1) s |>
  getNumLists in
  List.drop ((List.length s)-1) s |>
  List.hd |>
  process_input |>
  List.rev |>
  addList nums 

let get_input2 f = read_file f |> process_input2



let main () =
  let input = get_input2 file 
  in
  List.map printIntList input;
  print_newline();
  print_int(p2 input);
  print_newline();;

main()

  