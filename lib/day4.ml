let rgx = "Card.+?(\\d+): (.*) \\| (.*)"
let split_rgx = Re.Perl.compile_pat " +"

let calc_score list =
  let n = List.length list in
  Int.shift_left 1 (n-1)

let intersect list1 list2 =
  let sorted_list1 = List.sort compare list1 in
  let sorted_list2 = List.sort compare list2 in
  let rec aux res l1 l2 = match l1, l2 with
    | [], _ | _, [] -> res
    | h1::t1, h2::t2 -> match compare h1 h2 with
                          | (-1) -> aux res t1 l2
                          | 0 -> aux (h1::res) t1 t2
                          | 1 -> aux res l1 t2
                          | _ -> res
  in aux [] sorted_list1 sorted_list2

let parse_line rgx line = match Re.exec_opt rgx line with
  | None -> 0
  | Some m -> calc_score (
                intersect
                  (List.map int_of_string (Re.split split_rgx (Re.Group.get m 2)))
                  (List.map int_of_string (Re.split split_rgx (Re.Group.get m 3)))
              )

let sum_lines rgx lines =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (res + parse_line rgx hd) tl
  in aux 0 lines

let make_graph rgx lines =
  let make_node m =
    let id  = int_of_string (Re.Group.get m 1)
    in let n = List.length (
        intersect
          (List.map int_of_string (Re.split split_rgx (Re.Group.get m 2)))
          (List.map int_of_string (Re.split split_rgx (Re.Group.get m 3)))
    ) in let v= List.init n (fun x -> x+id+1)
    in (id, v)
  in
  let parse_line_2 rgx l = match Re.exec_opt rgx l with
    | None -> (-1, [])
    | Some m -> make_node m
  in
    let rec aux res = function
    | [] -> res
    | hd::tl -> aux ((parse_line_2 rgx hd)::res) tl
  in aux [] lines


let parcours g start =
    let rec get_voisin s = function
      | [] -> []
      | (i, v)::_ when i = s -> v
      | (i, _)::tl when i != s -> get_voisin s tl
      | (_, _)::_ -> []
    in
    let rec aux tovisit visited = match tovisit with
      | [] -> visited
      | hd::tl -> aux ((get_voisin hd g) @ tl) (hd::visited)
    in aux [start] []


let sum_lines_2 rgx lines =
  let gr = make_graph rgx lines in
  let n = List.length lines in
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux ((parcours gr hd) @ res) tl
  in List.length (aux [] (List.init n (fun x -> x+1)))


let results file =
  let r1 = sum_lines (Re.Perl.compile_pat rgx) (Common.read_lines file) in
  let r2 = sum_lines_2 (Re.Perl.compile_pat rgx) (Common.read_lines file) in
  Printf.sprintf "day4\tpt1: %d\tpt2: %d\n" r1 r2

