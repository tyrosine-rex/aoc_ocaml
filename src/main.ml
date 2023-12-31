let () =
  print_endline (Lib.Day1.results "./input/day1.txt");
  print_endline (Lib.Day2.results "./input/day2.txt");
  (* print_endline (Lib.Day4.results "./input/day4.txt"); part 2 is very slow (some seconds) --> need memoisation, multiple same traversal ! *)
  print_endline "day4\tSKIPPED --> \tWORK BUT SLOW";
  print_endline (Lib.Day6.results "./input/day6.txt");
  print_string (Lib.Day7.results "./input/day7.txt");
  print_endline (Lib.Day7pt2.results "./input/day7.txt");
  print_endline (Lib.Day9.results "./input/day9.txt");

  print_endline(Lib.Day19.results "./input/day19_1.txt" "./input/day19_2.txt")