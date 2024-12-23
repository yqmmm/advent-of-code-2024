open Core

type stats = {
    area: int;
    perimeter: int;
}

module Stats = struct
    let ( + ) a b = { area = a.area + b.area ; perimeter = a.perimeter + b.perimeter }
end

module Position = struct
    module T = struct
        type t = int * int [@@deriving compare, sexp_of]
    end
    include T
    include Comparator.Make(T)
end

let directions = [|(0,1); (0,-1); (1,0); (-1,0)|]

type direction = Horizontal | Vertical

type movement = East | West | North | South
[@@deriving equal]

type edge = {
    row: int;
    col: int;
    dir: direction;
    facing: movement;
}

module Edge = struct
    let compare a b =
        match a.dir, b.dir with
        | Horizontal, Vertical   -> -1
        | Vertical,   Horizontal -> 1
        | Horizontal, Horizontal -> if a.row = b.row then Int.compare a.col b.col else Int.compare a.row b.row
        | Vertical,   Vertical   -> if a.col = b.col then Int.compare a.row b.row else Int.compare a.col b.col

    let is_same_fence a b =
        match a.dir, b.dir with
        | Horizontal, Vertical   -> false
        | Vertical,   Horizontal -> false
        | Horizontal, Horizontal -> (a.row = b.row) && (equal_movement a.facing b.facing) && (abs (a.col - b.col) = 1)
        | Vertical,   Vertical   -> (a.col = b.col) && (equal_movement a.facing b.facing) && (abs (a.row - b.row) = 1)
end

module Movement = struct
    let all = [East; West; North; South]

    let to_delta = function
        | East  -> (0, 1)
        | West  -> (0, -1)
        | North -> (-1, 0)
        | South -> (1, 0)

    let to_edge r c = function
        | East ->  { row = r     ; col = c + 1 ; dir = Vertical   ; facing = East  }
        | West ->  { row = r     ; col = c     ; dir = Vertical   ; facing = West  }
        | North -> { row = r     ; col = c     ; dir = Horizontal ; facing = North }
        | South -> { row = r + 1 ; col = c     ; dir = Horizontal ; facing = South }
end

let part1 map =
    let rows = Array.length map in
    let cols = Array.length map.(0) in
    let within_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in

    let visited = ref (Set.empty (module Position)) in

    let rec search i j ch =
        if not (within_bounds i j) then { area = 0 ; perimeter = 0 }
        else if not Char.(map.(i).(j) = ch) then { area = 0 ; perimeter = 0 }
        else if Set.mem !visited (i, j) then { area = 0 ; perimeter = 0 }
        else (
            visited := Set.add !visited (i, j);
            let perimeter = Array.fold directions ~init:0 ~f:(fun acc (dx, dy) ->
                let nx = i + dx in
                let ny = j + dy in
                if not (within_bounds nx ny) || not Char.(map.(nx).(ny) = ch) then
                    acc + 1
                else
                    acc
            ) in
            Array.fold directions ~init:({ area = 1 ; perimeter }) ~f:(fun acc (dx, dy) ->
                let nx = i + dx in
                let ny = j + dy in
                Stats.(acc + (search nx ny ch))
            )
        )
    in

    let res = ref 0 in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let { area ; perimeter } = search i j map.(i).(j) in
            res := !res + area * perimeter
        done;
    done;

    !res

let part2 map =
    let rows = Array.length map in
    let cols = Array.length map.(0) in
    let within_bounds x y = x >= 0 && x < rows && y >= 0 && y < cols in

    let visited = ref (Set.empty (module Position)) in

    let rec search i j ch =
        if not (within_bounds i j) then (0, [])
        else if not Char.(map.(i).(j) = ch) then (0, [])
        else if Set.mem !visited (i, j) then (0, [])
        else (
            visited := Set.add !visited (i, j);
            List.fold Movement.all ~init:(1, []) ~f:(fun (area, edges) m ->
                let dx, dy = Movement.to_delta m in
                let nx, ny = i + dx, j + dy in
                if within_bounds nx ny && Char.(map.(nx).(ny) = ch) then
                    let a, e = search nx ny ch in
                    (area + a, e @ edges)
                else
                    (area, Movement.to_edge i j m :: edges)
            )
        )
    in

    let res = ref 0 in

    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let area, edges = search i j map.(i).(j) in
            let edges = List.dedup_and_sort edges ~compare:Edge.compare in
            let fences = List.group edges ~break:(fun a b -> not (Edge.is_same_fence a b)) in
            res := !res + area * List.length fences
        done;
    done;
    
    !res

let run filename =
    let map = Stdio.In_channel.read_lines filename |> List.map ~f:(String.to_array) |> List.to_array in
    Stdio.printf "%d\n" (part1 map);
    Stdio.printf "%d\n" (part2 map)

let () =
    run "./sample.txt";
    run "./input.txt";

