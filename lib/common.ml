let read_lines file =
  let ic = In_channel.open_text file in
  In_channel.input_lines ic

let rec print_int_list res = function
  | [] -> res
  | hd::tl -> print_int_list (res ^ "\n" ^ (string_of_int hd)) tl
