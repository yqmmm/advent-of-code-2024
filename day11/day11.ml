open Core

let blink_nonrecursive blink (stone, count) =
    if count = 0 then
        1
    else if stone = 0 then
        blink (1, (count - 1))
    else
        let s = Int.to_string stone in
        let len = String.length s in
        if len mod 2 = 0 then
            [ String.slice s 0 (len / 2); String.slice s (len / 2) len ]
            |> List.map ~f:Int.of_string
            |> List.fold ~init:0 ~f:(fun acc x -> acc + (blink (x, (count - 1))))
        else
            blink ((stone * 2024), (count - 1))

let blink = Memo.recursive ~hashable:Hashtbl.Poly.hashable blink_nonrecursive

let part1 stones =
    List.fold stones ~init:0 ~f:(fun acc stone -> acc + (blink (stone, 25)))

let part2 stones =
    List.fold stones ~init:0 ~f:(fun acc stone -> acc + (blink (stone, 75)))

let run filename =
    let line = List.hd_exn @@ Stdio.In_channel.read_lines filename in
    let stones = String.split_on_chars line ~on:[' '] |> List.map ~f:Int.of_string in
    Stdio.printf "%d\n" (part1 stones);
    Stdio.printf "%d\n" (part2 stones)

let () =
    run "./sample.txt";
    run "./input.txt";

