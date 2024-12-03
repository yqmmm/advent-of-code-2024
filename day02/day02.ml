open Core

let judge predicate lines =
  List.fold lines ~init:0 ~f:(fun acc line ->
    let levels = String.split_on_chars ~on:[' '] line |> List.map ~f:int_of_string in
    if predicate levels then
      acc + 1
    else
      acc
  )

let rec diff_within min max = function
  | [] | [_] -> true
  | x :: y :: rest -> (y - x >= min) && (y - x <= max) && diff_within min max (y :: rest)

let part1 = judge (fun levels -> diff_within 1 3 levels || diff_within (-3) (-1) levels)

let rec diff_within_remove min max remove = function
  | [] | [_] -> true
  | x :: y :: rest ->
      if (y - x >= min) && (y - x <= max) then
        diff_within_remove min max remove (y :: rest)
      else if remove > 0 then
        diff_within_remove min max (remove - 1) (x :: rest)
      else
        false

let diff_within_plus min max = function
  | [] | [_] -> true
  | x :: y :: rest ->
      diff_within min max (y :: rest) || diff_within_remove min max 1 ( x :: y :: rest)

let part2 = judge (fun levels -> diff_within_plus 1 3 levels || diff_within_plus (-3) (-1) levels)

let () =
  let lines = Stdio.In_channel.read_lines "./input.txt" in
  Stdio.printf "%d\n" (part1 lines);
  Stdio.printf "%d\n" (part2 lines)

