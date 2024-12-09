open Core

let parse line =
    let left, right = String.rsplit2_exn line ~on:':' |> Tuple2.map ~f:String.strip in
    (Int.of_string left), (String.split right ~on:' ' |> List.map ~f:Int.of_string)

let part1 lines =
    List.fold lines ~init:0 ~f:(fun acc line ->
        let target, numbers = parse line in

        let rec search current = function
            | [] -> current = target
            | x :: xs -> (search (current + x) xs)
                      || (search (current * x) xs)
        in
        acc + (if search 0 numbers then target else 0)
    )

let part2 lines =
    List.fold lines ~init:0 ~f:(fun acc line ->
        let target, numbers = parse line in

        let rec search current = function
            | [] -> current = target
            | x :: xs -> (search (current + x) xs)
                      || (search (current * x) xs)
                      || (search (Int.of_string ((Int.to_string current) ^ (Int.to_string x))) xs)
        in
        acc + (if search 0 numbers then target else 0)
    )

let run filename =
    let lines = Stdio.In_channel.read_lines filename in
    Stdio.printf "%d\n" (part1 lines);
    Stdio.printf "%d\n" (part2 lines)

let () =
    run "./sample.txt";
    run "./input.txt";

