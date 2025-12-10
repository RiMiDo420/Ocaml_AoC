let file = "day3_in.txt"

let processRow currentBest n =
let rec processNum currentBest n =
  match currentBest with 
  |[] -> []
  |h::h2::t -> (Int.max h2 ((10*h) + n))::(processNum (h2::t) n)
  |h::[] -> []
in
0::(processNum currentBest n)

let genList n =
  let rec listGen = function
  |0 -> []
  |n -> -1::(listGen (n-1))
in
0::listGen n

let rec getLast = function
|[] -> failwith("no elements in list")
|h::[] -> h
|h::t -> getLast t

let rec getVoltage maxLen row= 
  let out =List.fold_left processRow (genList maxLen) row |> getLast
in
print_int(out);
print_newline();
out

let p1 row = 
  (List.map (getVoltage 2) row) |> List.fold_left (fun acc x -> acc+x) 0 
  
let p2 row = 
  (List.map (getVoltage 12) row) |> List.fold_left (fun acc x -> acc+x) 0 


let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let processString s = String.fold_right(fun c acc-> (int_of_char c - int_of_char '0')::acc) s []

let getInput f = 
  read_file f |> List.map processString 
let main () =
  let input = getInput file 
  in
  print_newline();
  print_int(p2 input);
  print_newline();;

let getIntSize () =
  print_newline();
  print_int(Sys.int_size);
  print_newline();;

main();;
