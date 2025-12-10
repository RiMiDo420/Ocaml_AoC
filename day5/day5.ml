let file = "day5_in.txt"

let rec printRange = function
|[] -> print_newline(); ()
|(a, b)::t -> print_int(a); print_char('-'); print_int(b); print_newline(); printRange t

let rec addRange (s, e) = function
  |(ls, le)::t when e<(ls-1) -> (s, e)::(ls, le)::t
  |(ls, le)::t when e<le -> (Int.min s ls, le)::t
  |(ls, le)::t when s<=(le+1) -> addRange (min ls s, e) t 
  |(ls, le)::t when s>(le+1) -> (ls, le)::(addRange (s, e) t)
  |(ls, le)::t -> failwith("Pattern matching found nothing")
  |[] -> (s, e)::[]

let rec findNum n = function
|(s, e)::t when n<s -> false
|(s, e)::t when n<=e -> true
|(s, e)::t when n>e -> findNum n t
|(s, e)::t -> failwith("Find number pattern matching found nothing")
|[] -> false

let rec buildIntervalList = function
|[] -> []
|h::t -> addRange h (buildIntervalList t)

let p1 intervals ingredients =
  let interList = buildIntervalList intervals in printRange interList;
  List.filter (fun x -> findNum x interList) ingredients|>List.length

let p2 intervals ingredients =
  buildIntervalList intervals |>
   List.fold_left (fun acc (a, b) -> acc+(b-a+1)) 0

let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let rec process_input = function
  |[] -> failwith("No second part of input")
  |""::t -> ([], List.map (fun s -> int_of_string s) t)
  |h::t -> match process_input t with
          |(a, b) -> let nums = String.split_on_char '-' h in
            ((int_of_string (List.hd nums), int_of_string (List.hd (List.tl nums)))::a, b)
  
let getInput f = process_input (read_file f)


let main () =
  let input = getInput file 
  in
  print_newline();
  match input with
  (a, b) -> printRange a; print_int(p2 a b); print_newline();;

  

main()
  
