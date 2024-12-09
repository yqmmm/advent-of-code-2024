open Core

let find_two_antinodes (x, y) (x', y') rows cols =
    let in_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in
    let dx = x - x' in
    let dy = y - y' in

    let antinodes = ref (Set.Poly.empty) in

    if in_bounds (x + dx) (y + dy) then
        antinodes := Set.add !antinodes (x + dx, y + dy);
    if in_bounds (x' - dx) (y' - dy) then
        antinodes := Set.add !antinodes (x' - dx, y' - dy);

    !antinodes

let find_line_antinodes (x, y) (x', y') rows cols =
    let in_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in
    let dx = x - x' in
    let dy = y - y' in

    let antinodes = ref (Set.Poly.empty) in

    let rec add_antinodes a b da db =
        if in_bounds a b then begin
            antinodes := Set.add !antinodes (a, b);
            add_antinodes (a + da) (b + db) da db
        end
    in

    add_antinodes x y dx dy;
    add_antinodes x' y' (-dx) (-dy);

    !antinodes

let run map get_antinodes =
    let rows = Array.length map in
    let cols = Array.length map.(0) in

    let loc_map  = ref (Map.empty (module Char)) in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let c = map.(i).(j) in
            if Char.(c <> '.') then
                loc_map := Map.update !loc_map c ~f:(fun locations ->
                    Option.value locations ~default:[] |> List.append [ (i, j) ]
                )
        done;
    done;

    let antinodes = ref (Set.Poly.empty) in
    Map.iteri !loc_map ~f:(fun ~key:_ch ~data:locations ->
        List.iter locations ~f:(fun (x, y) ->
            List.iter locations ~f:(fun (x', y') ->
                if Poly.((x, y) <> (x', y')) then
                    antinodes := Set.union !antinodes (get_antinodes (x, y) (x', y') rows cols)
            )
        );
    );

    Set.length !antinodes

let part1 map = run map find_two_antinodes

let part2 map = run map find_line_antinodes

let run filename =
    let lines = Stdio.In_channel.read_lines filename in
    let map = Array.of_list (List.map lines ~f:(fun line -> String.to_array line)) in
    Stdio.printf "%d\n" (part1 map);
    Stdio.printf "%d\n" (part2 map)

let () =
    run "./sample.txt";
    run "./input.txt";

