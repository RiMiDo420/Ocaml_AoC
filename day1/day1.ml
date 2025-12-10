let file = "day1_in.txt"

let modulo x y = 
   let r = x mod y
    in
    if(r<0) then r+y else r
    

let rec rotate_dial pos rotations_l =
  match rotations_l with
  |[] -> []
  |(dir, num)::t when dir='L' -> let newPos = modulo (pos-num) 100 in
                                newPos::(rotate_dial newPos t)
  |(dir, num)::t when dir='R' -> let newPos = modulo (pos+num) 100 in
                                newPos::(rotate_dial newPos t)
  |_::_ -> failwith("shouldn't happen")


let rec rotateDumb pos rotations_l acc= 
  match rotations_l with
  |[] -> acc
  |(dir, num)::t when num>=100 -> rotateDumb pos ((dir, num mod 100)::t) ((num/100)+acc)
  |(dir, num)::t when num=0 -> rotateDumb pos t acc
  |(dir, num)::t when dir='L' -> let newPos = if pos-1 < 0 then pos-1+100 else pos-1 in
                                              if newPos = 0 then rotateDumb newPos ((dir, num-1)::t) (1+acc)
                                              else rotateDumb newPos ((dir, num-1)::t) (acc)
  |(dir, num)::t when dir='R' -> let newPos = if pos+1 > 99 then pos+1-100 else pos+1 in
                                              if newPos = 0 then rotateDumb newPos ((dir, num-1)::t) (1+acc)
                                              else rotateDumb newPos ((dir, num-1)::t) (acc)
  |_::_ -> failwith("shouldn't happen")

let p1 l =
  List.length (List.filter (fun x-> x=0) (rotate_dial 50 l))

let p2 l = rotateDumb 50 l 0


let read_file file_path =
  let file = In_channel.open_bin file_path in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  strings

let rec processStrings = function
  |[] -> []
  |h::t -> (h.[0], (int_of_string (String.sub h 1 ((String.length h) -1))))::(processStrings t)


let main () =
  let input  = processStrings (read_file file) in
  print_int(p2 input);;
  print_newline ();;

main ();;


