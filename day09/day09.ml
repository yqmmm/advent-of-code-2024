open Core

let expand s =
    let len = String.length s in
    let rec process idx blocks =
        if idx >= len then (List.rev blocks)
        else
            let n = int_of_char s.[idx] - int_of_char '0' in
            process
                (idx + 1)
                (
                    if idx mod 2 = 0 then
                        List.init n ~f:(fun _ -> idx / 2) @ blocks
                    else
                        List.init n ~f:(fun _ -> -1) @ blocks
                )
    in 
    process 0 [] |> List.to_array

let part1 line =
    let blocks = expand line in

    let i, j = (ref 0, ref (Array.length blocks - 1)) in

    while !i < !j do
        if blocks.(!i) <> -1 then
            i := !i + 1
        else if blocks.(!j) = -1 then
            j := !j - 1
        else (
            blocks.(!i) <- blocks.(!j);
            blocks.(!j) <- -1;
        )
    done;

    let sum = ref 0 in
    for i = 0 to Array.length blocks - 1 do
        if blocks.(i) <> -1 then
            sum := !sum + i * blocks.(i)
    done;

    !sum

type file = { idx: int; value: int; len: int; }

type blank = { idx: int; len: int; }

let part2 line =
    let rec parse files blanks idx len =
        if idx >= String.length line then
            (List.rev files), (List.rev blanks)
        else
            let n = int_of_char line.[idx] - int_of_char '0' in
            if idx mod 2 = 0 then
                let file = { idx = len; value = idx / 2; len = n } in
                parse (file :: files) blanks (idx + 1) (len + n)
            else
                let blank = { idx = len; len = n } in
                parse files (blank :: blanks) (idx + 1) (len + n)
    in

    let files, blanks = parse [] [] 0 0 in
    let blanks = Array.of_list blanks in

    let results = List.fold_right files ~init:[] ~f:(fun file acc ->
        (
            match Array.findi blanks ~f:(fun _ blank -> blank.idx < file.idx && blank.len >= file.len) with
            | None -> file
            | Some(idx, blank) ->
                let new_file = { file with idx = blank.idx } in
                blanks.(idx) <- { idx = blank.idx + file.len; len = blank.len - file.len };
                new_file
        ) :: acc
    ) in

    List.fold results ~init:0 ~f:(fun acc file ->
        acc + (file.idx + file.idx + file.len - 1) * file.len / 2 * file.value
    )

let run filename =
    let lines = Stdio.In_channel.read_lines filename in
    (* Input has only one line *)
    let line = List.hd_exn lines in
    Stdio.printf "%d\n" (part1 line);
    Stdio.printf "%d\n" (part2 line)

let () =
    run "./sample.txt";
    run "./input.txt";

