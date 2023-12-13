let rgx = "Card.+?(\\d+): (.*) \\| (.*)"

let split_line l = Re.split (Re.Perl.compile_pat " +") l

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
                (List.map int_of_string (split_line (Re.Group.get m 2)))
                (List.map int_of_string (split_line (Re.Group.get m 3)))
              )

let sum_lines rgx lines =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (res + parse_line rgx hd) tl
  in aux 0 lines

let results =
  let r1 = sum_lines (Re.Perl.compile_pat rgx) (Common.read_lines "./input/day4.txt") in
  Printf.sprintf "day4\tpt1: %d\tpt2: \n" r1
