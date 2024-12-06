open Core

let read_input lines =
  let rec process_lines rules pages has_empty_line = function
    | [] -> (List.rev rules, List.rev pages)
    | line :: rest ->
      if String.is_empty line then
        process_lines rules pages true rest
      else if not has_empty_line then
        (* Part 1: rules *)
        let rule = Scanf.sscanf line "%d | %d" (fun a b -> (a, b)) in
        process_lines (rule :: rules) pages has_empty_line rest
      else
        (* Part 2: pages *)
        let page = String.split ~on:',' line |> List.map ~f:int_of_string in
        process_lines rules (page :: pages) has_empty_line rest
  in
  let rules, pages = process_lines [] [] false lines in
  Map.of_alist_multi (module Int) rules, pages

(* evaluate if page satisfy all the orders in rule_map *)
let evaluate_page rule_map page =
    let rec evaluate_page' rule_map page seen =
        match page with
        | [] -> true
        | x :: xs -> (
            if Map.find rule_map x
               |> Option.value ~default:[]
               |> List.exists ~f:(fun item -> Set.mem seen item)
            then false else evaluate_page' rule_map xs (Set.add seen x)
        )
    in
    evaluate_page' rule_map page (Set.empty (module Int))

let middle_element lst =
  let len = List.length lst in
  if len = 0 then None
  else List.nth lst (len / 2)

let part1 lines = 
    let rule_map, pages = read_input lines in
    List.fold pages ~init:0 ~f:(fun acc page ->
        acc + (
            if evaluate_page rule_map page then
                (middle_element page |> Option.value ~default:0)
            else 0
        )
    )

let part2 lines =
    let rule_map, pages = read_input lines in

    (* DFS based topological sort *)
    let dfs page visited start_node =
        let rec explore path visited node =
            if List.mem path node ~equal:Int.equal then raise (Invalid_argument "cycle detected");
            if List.mem visited node ~equal:Int.equal then visited else
                let new_path = node :: path in
                let edges = Map.find rule_map node
                            |> Option.value ~default:[]
                            (* filter out nodes not on current page *)
                            |> List.filter ~f:(fun node -> List.mem page node ~equal:Int.equal) in
                let visited = List.fold edges ~init:visited ~f:(
                    fun acc edge -> explore new_path acc edge) in
                node :: visited
        in explore [] visited start_node
    in

    let topo_sort page =
        List.fold page ~init:[] ~f:(fun visited node -> dfs page visited node)
    in

    List.fold pages ~init:0 ~f:(
        fun acc page -> acc + (
            if evaluate_page rule_map page then 0
            else
                let sorted = topo_sort page in 
                (* Stdio.print_endline (String.concat ~sep:" " (List.map ~f:string_of_int page)); *)
                (* Stdio.print_endline (String.concat ~sep:" " (List.map ~f:string_of_int sorted)); *)
                middle_element sorted |> Option.value ~default:0
        )
    )

let run filename =
  let lines = Stdio.In_channel.read_lines filename in
  Stdio.printf "%d\n" (part1 lines);
  Stdio.printf "%d\n" (part2 lines)

let () =
  run "./sample.txt";
  run "./input.txt";

