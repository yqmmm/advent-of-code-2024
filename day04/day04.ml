open Core

let check_xmas map x y =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let target = [|'X'; 'M'; 'A'; 'S'|] in
  
  let directions = [|(0,1); (0,-1); (1,0); (-1,0); (1,1); (1,-1); (-1,1); (-1,-1)|] in

  let within_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in
  
  Array.count ~f:(fun (dx, dy) ->
    let valid = ref true in
    for i = 0 to 3 do
      let nx = x + i * dx in
      let ny = y + i * dy in
      if not (within_bounds nx ny) || not (Char.equal map.(nx).(ny) target.(i)) then
        valid := false
    done;
    !valid
  ) directions

let check_xmax_plus map x y =
  let rows = Array.length map in
  let cols = Array.length map.(0) in

  let within_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in

  let check_diagonal_mas x y (dx, dy) =
    if not (within_bounds (x - dx) (y - dy)) || not (within_bounds (x + dx) (y + dy)) then
      false
    else
      match map.(x - dx).(y - dy), map.(x).(y), map.(x + dx).(y + dy) with
      | 'M', 'A', 'S' -> true
      | 'S', 'A', 'M' -> true
      | _ -> false
  in

  if check_diagonal_mas x y (1, 1) && check_diagonal_mas x y (1, -1) then 1 else 0
    
let find_xmas_positions grid predicate =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  
  let result = ref 0 in
  
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
        result := !result + (predicate grid x y)
    done;
  done;
  
  !result

let part1 map =
    find_xmas_positions map check_xmas

let part2 map =
    find_xmas_positions map check_xmax_plus

let run filename =
  let lines = Stdio.In_channel.read_lines filename in
  let map = Array.of_list (List.map lines ~f:(fun line -> String.to_array line)) in
  Stdio.printf "%d\n" (part1 map);
  Stdio.printf "%d\n" (part2 map)

let () =
  run "./sample.txt";
  run "./input.txt";

