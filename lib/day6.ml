let regex_line = "(\\d+)"


let calc_winning_case_range time dist =
  let t, d = float_of_int (-time), float_of_int dist in
  let discr = t *. t -. 4. *. d in
  let get_branch dis =
    let root = Float.sqrt dis in (
        ((-.t) -. root) /. 2. ,
        ((-.t) +. root) /. 2.
     ) in
  match discr >= 0. with
    | false -> failwith "discr negatif"
    | true -> get_branch discr

let convert_to_nb_combin list =
  let rec aux res = function
    | [] -> res
    | (b1, b2)::tl ->let ib1 = int_of_float (b1 +. 1.) in
                      let ib2 = if float_of_int (int_of_float b2) = b2 then
                        int_of_float (b2 -. 1.)
                      else
                        int_of_float b2
                      in
                      let r = (ib2 - ib1) + 1 in
                      aux (r::res) tl
in aux [] list


let rec prod = function
  | [] -> 1
  | hd::tl -> hd * prod tl

let results =
  let lines = Common.split_with "\n" (Common.read_all "./input/day6.txt") in
  let times = List.map int_of_string (List.tl (Common.split_with " +" (List.nth lines 0))) in
  let dists = List.map int_of_string (List.tl (Common.split_with " +" (List.nth lines 1))) in
  let all = Common.zip times dists in
  let win_cases_range = List.map (fun (t, d) -> calc_winning_case_range t d) all in
  let win_cases_comb = convert_to_nb_combin win_cases_range in
  let r1 = prod win_cases_comb in

  let time_2 = int_of_string (String.concat "" (List.tl (Common.split_with " +" (List.nth lines 0)))) in
  let dist_2 = int_of_string (String.concat "" (List.tl (Common.split_with " +" (List.nth lines 1)))) in
  let wcr2 = calc_winning_case_range time_2 dist_2 in
  let wcc = convert_to_nb_combin (wcr2::[]) in
  let r2 = prod wcc in
  Printf.sprintf "day6\tpt1: %d\tpt2: %d\n" r1 r2





