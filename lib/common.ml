
let read_lines file =
  let ic = In_channel.open_text file in
  In_channel.input_lines ic
