open Core

let search map post_func =
    let rows = Array.length map in
    let cols = Array.length map.(0) in

    let directions = [|(0,1); (0,-1); (1,0); (-1,0)|] in
    let within_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in

    let rec search' x y =
        if map.(x).(y) = 9 then [(x, y)]
        else
            Array.fold directions ~init:[] ~f:(fun acc (dx, dy) ->
                let nx = x + dx in
                let ny = y + dy in
                if within_bounds nx ny && map.(nx).(ny) - map.(x).(y) = 1 then
                    (search' nx ny) @ acc
                else
                    acc
            )
    in

    let count = ref 0 in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            if map.(i).(j) = 0 then
                let target = (search' i j) in
                count := !count + (List.length (post_func target))
        done;
    done;
    !count

let part1 map = search map (
    List.dedup_and_sort ~compare:(fun (x1, y1) (x2, y2) ->
        if x1 = x2 then y1 - y2 else x1 - x2
    )
)

let part2 map = search map (fun target -> target)

let run filename =
    let lines = Stdio.In_channel.read_lines filename in
    let map = Array.of_list (List.map lines ~f:(
        fun line -> 
            String.to_list line 
            |> List.map ~f:(fun ch -> Char.to_string ch |> Int.of_string)
            |> Array.of_list
    )) in

    Stdio.printf "%d\n" (part1 map);
    Stdio.printf "%d\n" (part2 map)

let () =
    run "./sample.txt";
    run "./input.txt";

