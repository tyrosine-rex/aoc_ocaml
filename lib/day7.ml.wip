let custom_compare a b = 
  let to_int = function 
    | "K" -> 13
    | "Q" -> 12
    | "J" -> 11
    | "T" -> 10 
    | x -> int_of_string x
  in compare (to_int a) (to_int b) 

let calc_spectrum_of_hand hand =
  let sorted_hand = List.sort custom_compare (Common.split_with "" hand) in 
  sorted_hand