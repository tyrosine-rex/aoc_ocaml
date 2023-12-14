let read_lines file =
  In_channel.input_lines (In_channel.open_text file)

let read_all file =
  In_channel.input_all (In_channel.open_text file)  

let rec print_int_list res = function
  | [] -> res
  | hd::tl -> print_int_list (res ^ "\n" ^ (string_of_int hd)) tl

let range st ed = match st < ed with 
  | true -> List.init (ed-st) (fun x -> x+st)
  | false -> List.rev (List.init (st-ed) (fun x -> x+ed+1))

let split_with pat s = 
  Re.split (Re.Perl.compile_pat pat) s

let zip l1 l2 =
  let rec aux res l1 l2 = match l1, l2 with 
    | [], _ | _, [] -> res 
    | h1::t1, h2::t2 -> aux ((h1, h2)::res) t1 t2
  in List.rev (aux [] l1 l2)