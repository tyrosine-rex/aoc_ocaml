open Common

type part = {
  x: int; 
  m: int;
  a: int;
  s: int;
}

type instruction = 
  | Accept
  | Reject
  | Ref of string
  | Test of ((part -> bool) * instruction * instruction)

let rgx_tokenize = Re.Perl.compile_pat "\\{|\\}|:|,"
let rgx_compare = Re.Perl.compile_pat "(.*)(<|>)(.*)"


let part_is op vl key p = match op, key with
  | '>', 'x' -> p.x > vl
  | '<', 'x' -> p.x < vl
  | '>', 'm' -> p.m > vl
  | '<', 'm' -> p.m < vl
  | '>', 'a' -> p.a > vl
  | '<', 'a' -> p.a < vl
  | '>', 's' -> p.s > vl
  | '<', 's' -> p.s < vl
  | _ -> failwith "key inconnu"
  
let make_instruction tokens = 
  let analyze_compare comp = 
    let m = Re.exec rgx_compare comp in 
    let op = (Re.Group.get m 2).[0] in 
    let vl = int_of_string (Re.Group.get m 3) in 
    let key = (Re.Group.get m 1).[0] in 
    part_is op vl key
  in 
  let analyze_ref = function 
    | "A" -> Accept 
    | "R" -> Reject 
    | x -> Ref(x)
  in
  let rec aux = function 
    | comp::ref::ref2::[] -> Test( analyze_compare comp, analyze_ref ref, analyze_ref ref2)
    | comp::ref::tl -> Test( analyze_compare comp, analyze_ref ref, aux tl)
    | _ -> failwith "list bizarre"
  in 
  aux tokens

let generate_key_value l = 
  let llist = Re.split rgx_tokenize l in 
  List.hd llist, List.tl llist

let populate_dico lines = 
  let dico = Hashtbl.create 1024 in 
  let rec aux = function 
    | [] -> ()
    | (k, v)::tl -> Hashtbl.add dico k (make_instruction v); aux tl
  in aux (List.map generate_key_value lines);
  dico 

let rec remove_ref dico = function
  | Ref s -> remove_ref dico (Hashtbl.find dico s)
  | Accept -> Accept
  | Reject -> Reject 
  | Test(fn,y,n) -> Test(fn, remove_ref dico y, remove_ref dico n)

let rec eval_tree part = function 
  | Accept -> Accept 
  | Reject -> Reject 
  | Test(fn, y, n) -> if fn part then eval_tree part y else eval_tree part n 
  | Ref _  -> failwith "en theorie il n'y a pas de Ref<instruction>"


let rgx_part_x = Re.Perl.compile_pat "x=(\\d+)"
let rgx_part_m = Re.Perl.compile_pat "m=(\\d+)"
let rgx_part_a = Re.Perl.compile_pat "a=(\\d+)"
let rgx_part_s = Re.Perl.compile_pat "s=(\\d+)"  

let read_part l = {
  x=int_of_string (Re.Group.get (Re.exec rgx_part_x l) 1);
  m=int_of_string (Re.Group.get (Re.exec rgx_part_m l) 1);
  a=int_of_string (Re.Group.get (Re.exec rgx_part_a l) 1);
  s=int_of_string (Re.Group.get (Re.exec rgx_part_s l) 1)
}




let results file file2 = 
  let lines = read_lines file in 
  let parts = List.map read_part (read_lines file2) in
  let dico = populate_dico lines in 
  let tree = remove_ref dico (Hashtbl.find dico "in") in 
  let acceptes = List.filter (fun x -> match (eval_tree x tree) with | Accept -> true | Reject -> false | _ -> false) parts in 
  let rec sum = function 
    | [] -> 0 
    | hd::tl -> hd.x + hd.s + hd.a + hd.m + sum tl
  in sum acceptes
    |> Printf.sprintf "day19 pt1: %d \n" 

  
