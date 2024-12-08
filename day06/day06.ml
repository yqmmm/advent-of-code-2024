open Core

type tile = Empty | Obstacle | Start
[@@deriving equal]

let parse_map (lines : string list) =
    let map =
        List.map lines ~f:
        (fun line ->
            String.to_list line
            |> List.map ~f:(
                fun ch ->
                    match ch with
                    | '#' -> Obstacle
                    | '^' -> Start
                    | _ -> Empty)
            |> Array.of_list
        ) |> Array.of_list
    in

    let rows = Array.length map in
    let cols = Array.length map.(0) in

    let start = ref (0, 0) in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            match map.(i).(j) with
            | Start -> start := (i, j)
            | _ -> ()
        done;
    done;

    map, !start

type direction = Up | Down | Left | Right
[@@deriving equal]

let direction_turn = function
    | Up    -> Right
    | Right -> Down
    | Down  -> Left
    | Left  -> Up

let direction_delta = function
    | Up    -> (-1, 0)
    | Right -> (0,  1)
    | Down  -> (1,  0)
    | Left  -> (0, -1)

type state = { direction : direction ; position : int * int }
[@@deriving equal]

let next_position (r, c) direction =
    let (dr, dc) = direction_delta direction in
    (r + dr, c + dc)

let simulate map start =
    let rows = Array.length map in
    let cols = Array.length map.(0) in

    let rec step visited current =
        let {direction ; position = (r, c)} = current in
        let (nr, nc) = next_position (r, c) direction in

        if nr < 0 || nr >= rows || nc < 0 || nc >= cols then
            visited, true
        else
            let new_state = match map.(nr).(nc) with
                | Obstacle -> 
                    let new_direction = direction_turn direction in
                    {direction = new_direction; position = (r, c)}
                | _ ->
                    {direction = direction; position = (nr, nc)}
            in

            if Set.mem visited new_state then
                visited, false
            else
                step (Set.add visited new_state) new_state
    in

    step (Set.Poly.of_list [start]) start

let part1 map start =
    let path, _ = simulate map {direction = Up; position = start} in
    let tuple_equal (r1, c1) (r2, c2) = r1 = r2 && c1 = c2 in
    let uniq_positions = Set.fold path ~init:[] ~f:(fun acc state ->
        if List.mem acc state.position ~equal:tuple_equal then acc
        else state.position :: acc
    ) in
    (* Stdio.print_endline (String.concat ~sep:" " *)
    (*     (List.map ~f:(fun (r, c) -> Printf.sprintf "(%d, %d)" r c) uniq_positions)); *)
    List.length uniq_positions

let _part2 map start =
    let rows = Array.length map in
    let cols = Array.length map.(0) in

    let result = ref 0 in

    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            if equal_tile map.(i).(j) Empty then
                (* Stdio.printf "Tesging %d %d\n" i j; *)
                Out_channel.flush stdout;
                let original = map.(i).(j) in
                map.(i).(j) <- Obstacle;
                let _, out = simulate map {direction = Up ; position = start} in
                if not out then
                    (* Stdio.printf "Testing %d %d\n" i j; *)
                    result := !result + 1;
                map.(i).(j) <- original
        done;
    done;

    !result

let run filename =
    let lines = Stdio.In_channel.read_lines filename in
    let map, start = parse_map lines in
    Stdio.printf "%d\n" (part1 map start);
    Stdio.printf "%d\n" (_part2 map start)

let () =
    run "./sample.txt";
    run "./input.txt";

