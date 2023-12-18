type hand = 
  | Nothing
  | OnePair
  | TwoPair 
  | ThreeOf
  | FullHouse
  | FourOf 
  | FiveOf 

type card = 
  | Two| Three | Four 
  | Five | Six | Seven 
  | Eight | Nine | Ten 
  | Jack | Queen | King | As 

type player = {
  hand: hand; 
  cards: card list;
  bid: int;
}

let listnth nth list = List.nth list nth 

let str_to_card s = match s with
  | "2" -> Two
  | "3" -> Three 
  | "4" -> Four 
  | "5" -> Five 
  | "6" -> Six 
  | "7" -> Seven 
  | "8" -> Eight 
  | "9" -> Nine 
  | "T" -> Ten 
  | "J" -> Jack 
  | "Q" -> Queen 
  | "K" -> King 
  | "A" -> As 
  | _ -> raise Not_found

let group_by_card h_list = 
  let rec aux res grp = function
    | [] -> grp::res
    | hd::tl -> if hd = (List.hd grp) then 
                  aux res (hd::grp) tl 
                else 
                  aux (grp::res) [hd] tl 
  in aux [] [List.hd h_list] (List.tl h_list)

let hand_inference cards = 
  cards 
    |> List.sort compare
    |> group_by_card
    |> List.map List.length
    |> List.filter (fun x -> x > 1)
    |> function 
      | [2] -> OnePair
      | [3] -> ThreeOf 
      | [4] -> FourOf 
      | [5] -> FiveOf
      | [2; 3] | [3; 2] -> FullHouse
      | [2; 2] -> TwoPair
      | _ -> Nothing

let line_to_player line = 
  let splitted = line 
    |> String.trim 
    |> Common.split_with " +" in 
  let bid = splitted 
    |> listnth 1 
    |> int_of_string in 
  let cards = splitted 
    |> listnth 0 
    |> Common.split_with "" 
    |> List.map str_to_card in 
  let hand = hand_inference cards in 
    { hand=hand; cards=cards; bid=bid }

let comp_players ply1 ply2 = 
  if ply1.hand = ply2.hand then 
    compare ply1.cards ply2.cards 
  else
    compare ply1.hand ply2.hand

let sum_bid_rank plyrs = 
  let rec aux i = function 
    | [] -> 0
    | hd::tl -> (hd.bid * i) + aux (i+1) tl
  in aux 1 plyrs

let results file =
  Common.read_lines file 
    |> List.map line_to_player 
    |> List.sort comp_players
    |> sum_bid_rank
    |> Printf.sprintf "day7\tpt1: %d\t" 