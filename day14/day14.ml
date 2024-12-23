open Core

type robot = {
    x : int;
    y : int;
    dx : int;
    dy : int;
}

let parse_robot line =
    let pattern = Re.Perl.(compile (re {|p=([-]?\d+),([-]?\d+) v=([-]?\d+),([-]?\d+)|})) in
    let m = Re.exec pattern line in
    {
        x = (Re.Group.get m 1 |> Int.of_string) ;
        y = (Re.Group.get m 2 |> Int.of_string) ;
        dx = (Re.Group.get m 3 |> Int.of_string) ;
        dy = (Re.Group.get m 4 |> Int.of_string)
    }

let go (xlimit, ylimit) robot step =
    { robot with x = (robot.x + robot.dx * step) % xlimit; y = (robot.y + robot.dy * step) % ylimit }

type quadrant = LU | RU | LD | RD | NA

let categorize (x, y) (xlimit, ylimit) =
    let categorize' v limit =
        if limit % 2 = 0 then
            if v < limit / 2 then -1
            else 1
        else
            if v < limit / 2 then -1
            else if v = limit / 2 then 0
            else 1
    in
    match (categorize' x xlimit, categorize' y ylimit) with
    | (-1, -1) -> LU
    | (-1, 1) -> LD
    | (1, -1) -> RU
    | (1, 1) -> RD
    | _ -> NA

let quadrant_to_int = function
    | LU -> 0
    | RU -> 1
    | LD -> 2
    | RD -> 3
    | NA -> 4

let part1 robots limits =
    let cnt = Array.create ~len:5 0 in
    List.iter robots ~f:(fun robot ->
        let new_robot = go limits robot 100 in
        let q = categorize (new_robot.x, new_robot.y) limits in
        cnt.(quadrant_to_int q) <- cnt.(quadrant_to_int q) + 1;
    );
    (Array.fold cnt ~init:1 ~f:( * )) / cnt.(4)

let part2 robots (xlimit, ylimit) =
    for n = 0 to (xlimit * ylimit) do
        let map = Array.make_matrix ~dimx:xlimit ~dimy:ylimit 0 in

        let print_map () =
            printf " - %d - \n" n;
            for i = 0 to xlimit - 1 do
                for j = 0 to ylimit - 1 do
                    printf "%c" (if map.(i).(j) > 0 then 'X' else '.')
                done;
                printf "\n"
            done;
            printf "\n";
        in

        let max_consecutive () =
            let cnt = ref 0 in
            let ans = ref 0 in
            for i = 0 to xlimit - 1 do
                for j = 0 to ylimit - 1 do
                    (
                        if map.(i).(j) > 0 then
                            cnt := !cnt + 1
                        else
                            cnt := 0
                    );
                    ans := max !ans !cnt
                done;
            done;
            !ans
        in

        List.iter robots ~f:(fun robot ->
            let new_robot = go (xlimit, ylimit) robot n in
            (* printf " XXX %d %d\n" new_robot.x new_robot.y; *)
            map.(new_robot.x).(new_robot.y) <- map.(new_robot.x).(new_robot.y) + 1;
        );

        if (max_consecutive () > 8) then (
            print_map ();
        )

    done;
    0

let run filename limits do_part2 =
    let robots =
        Stdio.In_channel.read_lines filename
        |> List.map ~f:parse_robot
    in
    Stdio.printf "%d\n" (part1 robots limits);
    if do_part2 then Stdio.printf "%d\n" (part2 robots limits)

let () =
    run "./sample.txt" (11, 7) false;
    run "./input.txt" (101, 103) true;
