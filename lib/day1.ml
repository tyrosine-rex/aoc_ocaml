let rgx1 = "(\\d)(?:.*(\\d))?"
let rgx2 = "(\\d|zero|one|two|three|four|five|six|seven|eight|nine)(?:.*(\\d|zero|one|two|three|four|five|six|seven|eight|nine))?"

let digit_of_litteral l =
  match l with
    | "zero"  | "0" -> "0"
    | "one"   | "1" -> "1"
    | "two"   | "2" -> "2"
    | "three" | "3" -> "3"
    | "four"  | "4" -> "4"
    | "five"  | "5" -> "5"
    | "six"   | "6" -> "6"
    | "seven" | "7" -> "7"
    | "eight" | "8" -> "8"
    | "nine"  | "9" -> "9"
    | _ -> ""

let parse_line rgx line = match Re.exec_opt rgx line with
  | None -> 0
  | Some m -> match Re.Group.get_opt m 1, Re.Group.get_opt m 2 with
                | None, _ -> 0
                | Some g, None -> int_of_string (digit_of_litteral g ^ digit_of_litteral g)
                | Some g1, Some g2 -> int_of_string (digit_of_litteral g1 ^ digit_of_litteral g2)

let sum_lines rgx lines =
  let rec aux res = function
    | [] -> res
    | hd::tl -> aux (res + parse_line rgx hd) tl
  in aux 0 lines

let results =
  let r1 = sum_lines (Re.Perl.compile_pat rgx1) (Common.read_lines "./input/day1.txt") in
  let r2 = sum_lines (Re.Perl.compile_pat rgx2) (Common.read_lines "./input/day1.txt") in
  Printf.sprintf "day1\tpt1: %d\tpt2: %d\n" r1 r2
