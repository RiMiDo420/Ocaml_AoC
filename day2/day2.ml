let file = "day2_in.txt"

let readNum file= 
  let rec read file acc =
    let c_opt = In_channel.input_char file in
    match c_opt with
    |None -> acc
    |Some (c) -> if (Char.code c) <= (Char.code '9') && (Char.code c) >= (Char.code '0') then read file (10*acc +(int_of_char c))
    else acc
  in
  read file 0
let read_file file_path =
  let file = In_channel.open_bin file_path in
  let out = In_channel.input_all file in
  In_channel.close file;
  out

  let processPair p = 
    let nums = String.split_on_char '-' p in
    match nums with
    |h::t::[] -> (Int64.of_string h, Int64.of_string t)
    |_ -> failwith("invalid input format")
  let process_input raw = 
    List.map(processPair) (String.split_on_char ',' raw)


let rec pow x = function
 |0L -> 1L
 |p -> Int64.mul x (pow x (Int64.sub p 1L))
let generateDivisors len =
  let rec getDivisor (count:int64) (size:int64)  (acc:int64)  = 
    if count = 0L then acc
    else getDivisor (Int64.sub count 1L) size (Int64.add (Int64.mul (pow 10L size) acc) 1L)
  in
  let rec generateDivisor len i =
    if i = len then []
    else if Int64.rem len i = 0L then (print_string(Int64.to_string (len));
  print_newline();(getDivisor (Int64.div len i) i 0L)::generateDivisor len (Int64.add i 1L))
    else generateDivisor len (Int64.add i 1L)
  in
  generateDivisor len 1L

let generateDivisors_1 len =
  let rec getDivisor (count:int64) (size:int64)  (acc:int64)  = 
    if count = 0L then acc
    else getDivisor (Int64.sub count 1L) size (Int64.add (Int64.mul (pow 10L size) acc) 1L)
  in
  let rec generateDivisor len = (getDivisor 2L (Int64.div len 2L) 0L)::[]
  in
  if Int64.rem len 2L = 1L then []
  else generateDivisor len 

let rec genList s e =
  if s=e then []
  else (Int64.add s 1L)::genList (Int64.add s 1L) e

  let rec evalDivisors f s = function
|[] ->[]
|h::t -> 
  print_string(Int64.to_string (h));
  print_newline();
List.map (fun x -> Int64.mul h x) (genList (Int64.div (Int64.sub f 1L) h) (Int64.div s h)) @ evalDivisors f s t

let rec log_10 = function
|10L-> 1L
|0L -> -1L
|x -> Int64.add 1L (log_10 (Int64.div x 10L))
let rec generateInvalid f s =
  let lf = log_10 f
  in
  let ls = log_10 s
  in
  if not (Int64.equal lf ls) then Int64.add (generateInvalid f (Int64.sub (pow 10L ls) 1L)) (generateInvalid (pow 10L ls) s)
  else List.fold_left (fun acc x -> Int64.add acc x) 0L (List.sort_uniq Int64.compare (evalDivisors f s (generateDivisors (Int64.add lf 1L))))

let rec generateInvalid_1 f s :int64 =
  print_string(Int64.to_string (f));
  print_newline();
  print_string(Int64.to_string (s));
  print_newline();

  let lf = log_10 f
  in
  let ls = log_10 s
  in
  if not (Int64.equal lf ls) then Int64.add (generateInvalid_1 f (Int64.sub (pow 10L ls) 1L)) (generateInvalid_1 (pow 10L ls) s)
  else List.fold_left (fun acc x -> Int64.add acc x) 0L (List.sort_uniq Int64.compare (evalDivisors f s (generateDivisors_1 (Int64.add lf 1L))))

let unpair = function (a, b) -> generateInvalid a b

let unpair_1 = function (a, b) -> generateInvalid_1 a b
let rec p2 input = 
  List.fold_left (fun acc x -> Int64.add acc x) 0L (List.map unpair input)

  let rec p1 input = 
  List.fold_left (fun acc x -> Int64.add acc x) 0L (List.map unpair_1 input)

  let main () =
  let input  = process_input (read_file file) in
  print_string(Int64.to_string (p2 input));;
  print_newline ();;


main();;



