open Core

let extract_numbers lines =
  List.fold lines ~init:([], []) ~f:(fun (ax, bx) line ->
    match String.rsplit2 ~on:' ' line with
    | Some (a, b) ->
      (int_of_string (String.strip a) :: ax, int_of_string (String.strip b) :: bx)
    | None -> (ax, bx)
  )

let part1 lines =
  let ax, bx = extract_numbers lines |> Tuple2.map ~f:(List.sort ~compare:Int.compare) in
  List.fold2_exn ax bx ~init:0 ~f:(fun acc a b -> acc + abs (a - b))

let build_frequency_map =
  List.fold ~init:(Map.empty (module Int)) ~f:(fun acc num ->
    Map.update acc num ~f:(fun count -> Option.value count ~default:0 + 1)
  )

let part2 lines =
  let ax, bx = extract_numbers lines in
  let freq_map = build_frequency_map bx in
  List.fold ax ~init:0 ~f:(fun acc a ->
    acc + a * Option.value (Map.find freq_map a) ~default:0
  )

let () =
  let lines = Stdio.In_channel.read_lines "./input.txt" in
  Stdio.printf "%d\n" (part1 lines);
  Stdio.printf "%d\n" (part2 lines)

