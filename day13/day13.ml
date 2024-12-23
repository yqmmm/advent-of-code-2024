open Core

type button = {
    x : int;
    y : int;
}

type machine = {
    a : button;
    b : button;
    prize : button;
}

let parse_button line =
    let pattern = Re.Perl.(compile (re {|Button \w: X\+(\d+), Y\+(\d+)|})) in
    let m = Re.exec pattern line in
    { x = (Re.Group.get m 1 |> Int.of_string) ; y = (Re.Group.get m 2 |> Int.of_string) }

let parse_prize line =
    let pattern = Re.Perl.(compile (re {|Prize: X=(\d+), Y=(\d+)|})) in
    let m = Re.exec pattern line in
    { x = (Re.Group.get m 1 |> Int.of_string) ; y = (Re.Group.get m 2 |> Int.of_string) }

let parse_machine section =
    let lines = String.split_lines section |> Array.of_list in
    let a = parse_button (lines.(0)) in
    let b = parse_button (lines.(1)) in
    let prize = parse_prize (lines.(2)) in
    { a ; b ; prize }

(* ax * m + bx * n = prize_x *)
(* ay * m + by * n = prize_y *)

(* ax * by * m + bx * by * n = prize_x * by *)
(* ay * bx * m + bx * by * n = prize_y * bx *)
(* (ax * by - ay * bx) * m = prize_x * by - prize_y * bx *)
(* m = (prize_x * by - prize_y * bx) / (ax * by - ay * bx) *)

(* ax * ay * m + ay * bx * n = prize_x * ay *)
(* ax * ay * m + ax * by * n = prize_y * ax *)
(* (ay * bx - ax * by) * n = prize_x * ay - prize_y * ax *)
(* n = (prize_x * ay - prize_y * ax) / (ay * bx - ax * by) *)
(* n = (prize_y * ax - prize_x * ay) / (ax * by - ay * bx) *)

let solve { a ; b ; prize } =
    let denom = a.x * b.y - a.y * b.x in
    let m = (prize.x * b.y - prize.y * b.x) in
    let n = (prize.y * a.x - prize.x * a.y) in
    if Int.rem m denom = 0 && Int.rem n denom = 0 then (
        (m / denom) * 3 + (n / denom)
    ) else (
        0
    )

let part1 machines =
    List.fold ~init:0 ~f:(fun acc machine ->
        acc + (solve machine)
    ) machines

let part2 machines =
    List.fold ~init:0 ~f:(fun acc { a ; b ; prize } ->
        acc + (solve { a ; b ; prize = {
            x = prize.x + 10000000000000 ; y = prize.y + 10000000000000
        }})
    ) machines

let run filename =
    let machines =
        Stdio.In_channel.read_all filename
        |> Str.split (Str.regexp "\n\n")
        |> List.map ~f:parse_machine
    in
    Stdio.printf "%d\n" (part1 machines);
    Stdio.printf "%d\n" (part2 machines)

let () =
    run "./sample.txt";
    run "./input.txt"
