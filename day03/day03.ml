open Core

type item = 
  | Do 
  | Dont 
  | Mul of int * int

let extract_items s =
    let combined_pattern = Re.Perl.(compile (re {|do\(\)|don't\(\)|mul\((\d+),(\d+)\)|})) in
  
  let matches = Re.all combined_pattern s in
  
  List.map matches ~f:(fun m ->
    match Re.Group.get m 0 with
    | "do()" -> Do
    | "don't()" -> Dont
    | _ -> 
        let x = Re.Group.get m 1 |> Int.of_string in
        let y = Re.Group.get m 2 |> Int.of_string in
        Mul (x, y)
  )

let part1 lines = 
    List.fold lines ~init:0 ~f:(fun acc line ->
        let items = extract_items line in
        List.fold items ~init:acc ~f:(fun acc item -> acc + match item with
            | Do -> 0
            | Dont -> 0
            | Mul (x, y) -> x * y
        )
    )

let part2 lines = 
    List.fold lines ~init:(0, true) ~f:(fun acc line ->
        let items = extract_items line in
        List.fold items ~init:acc ~f:(fun (sum, enable) item ->
            match item with
            | Do -> (sum, true)
            | Dont -> (sum, false)
            | Mul (x, y) -> ((sum + if enable then x * y else 0), enable)
        )
    ) |> Tuple2.get1

let run filename =
  let lines = Stdio.In_channel.read_lines filename in
  Stdio.printf "%d\n" (part1 lines);
  Stdio.printf "%d\n" (part2 lines)

let () =
  run "./sample.txt";
  run "./input.txt";

