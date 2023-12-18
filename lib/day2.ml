type color = Red | Green | Blue

type game = {
  id: int;
  sets: (int * color) list list;
}

let rgx_id = Re.Perl.compile_pat "Game (\\d+):"
let rgx_tok = Re.Perl.compile_pat "(\\d+) (green|red|blue)"

let color_of_string s = match s with
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> failwith "mauvaise couleur"

let tokenize set_str =
  let toks = Re.matches rgx_tok set_str in
  let convert tk =
    int_of_string (List.nth (Common.split_with " " tk) 0),
    color_of_string (List.nth (Common.split_with " " tk) 1)
  in List.map convert toks

let parse_sets line =
  let sets = Common.split_with  "; |Game \\d+: " line in
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (tokenize hd::res) tl
in aux [] sets

let parse_line line = {
  id = int_of_string (Re.Group.get (Re.exec rgx_id line) 1);
  sets = parse_sets line
}

let is_possible_game r g b game =
  let is_possible_sets sets =
    let rec aux = function
      | [] -> true
      | (n, c)::tl when c = Red && n <= r     -> aux tl
      | (n, c)::tl when c = Green && n <= g   -> aux tl
      | (n, c)::tl when c = Blue && n <= b   -> aux tl
      | _ -> false
    in aux sets
  in let rec aux2 = function
    | [] -> true
    | hd::tl -> if is_possible_sets hd then aux2 tl else false
in aux2 game.sets

let rec sum_id = function
  | [] -> 0
  | hd::tl -> hd.id + sum_id tl

let rec max_of_l r color = function
  | (n, c)::tl when c = color -> if n > r then max_of_l n color tl else max_of_l r color tl
  | [] -> r
  | _::tl -> max_of_l r color tl

let calc_power game =
  let flat_set = List.flatten game.sets in
  let max_red = max_of_l 0 Red flat_set in
  let max_green = max_of_l 0 Green flat_set in
  let max_blue = max_of_l 0 Blue flat_set in
  max_red * max_green * max_blue

let rec sum = function
  | [] -> 0
  | hd::tl -> hd+(sum tl)

let results file =
  let lines = Common.read_lines file in
  let games = List.map parse_line lines in
  let filtered_games = List.filter (is_possible_game 12 13 14) games in
  let r1 = sum_id filtered_games in
  let powers = List.map calc_power games in
  let r2 = sum powers in
  Printf.sprintf "day2\tpt1: %d\tpt2: %d" r1 r2

