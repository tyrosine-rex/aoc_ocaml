let line_to_list line =
  List.map (fun x -> int_of_string x) (Common.split_with " +" line)

let rec variation_of = function
  | [] | _::[] -> []
  | a::b::tl -> (b-a) :: variation_of (b::tl)

let rec is_zeros_list = function
  | 0::[] -> true
  | hd::tl when hd = 0 -> is_zeros_list tl
  | _ -> false

let analyse_n_predict_next list =
  let analyse_list list =
    let rec aux res li = match is_zeros_list li with
      | true -> res
      | false -> let var = (variation_of li) in
                aux ((List.rev var)::res) var
    in aux [List.rev list] list
  in let predict_next a_list =
    let rec aux = function
      | [] -> 0
      | hd::tl -> (List.hd hd) + aux tl
    in aux a_list
  in let al = analyse_list list in predict_next al

let rec sum = function
  | [] -> 0
  | hd::tl -> hd + sum tl

let results file =
  let lines = Common.read_lines file in
  let lines_list = List.map line_to_list lines in
  let predicted = List.map analyse_n_predict_next lines_list in
  let predicted2 = List.map analyse_n_predict_next (List.map List.rev lines_list) in
  let r1 = sum predicted in
  let r2 = sum predicted2 in
  Printf.sprintf "day9\tpt1: %d\tpt2: %d\n" r1 r2