type position = { line: int; column: int } [@@warning "-69"]

type piece_color = Black | White | None [@@warning "-37"]

type piece = { position: position; color: piece_color; symbol: string } [@@warning "-69"]

type table = { piece_matrix: piece array array }

let king_piece_black =
  { position = { line = 0; column = 0}; color = Black; symbol = "K" }

let tower_piece_black =
  { position = { line = 0; column = 0 }; color = Black; symbol = "T"}

let bishop_piece_black =
  { position = { line = 0; column = 0 }; color = Black; symbol = "B"}

let queen_piece_black =
  { position = { line = 0; column = 0 }; color = Black; symbol = "Q"}

let knight_piece_black =
  { position = { line = 0; column = 0}; color = Black; symbol = "N" }

let pawn_piece_black =
  { position = { line = 0; column = 0}; color = White; symbol = "P"}

let king_piece_white =
  { position = { line = 0; column = 0}; color = White; symbol = "K" }

let tower_piece_white =
  { position = { line = 0; column = 0 }; color = White; symbol = "T"}

let bishop_piece_white =
  { position = { line = 0; column = 0 }; color = White; symbol = "B"}

let queen_piece_white =
  { position = { line = 0; column = 0 }; color = White; symbol = "Q"}

let knight_piece_white =
  { position = { line = 0; column = 0}; color = White; symbol = "N" }

let pawn_piece_white =
  { position = { line = 0; column = 0}; color = White; symbol = "P"}

let default_position = { line = 0; column = 0}

let default_piece = { position = default_position; color = None; symbol = "-" }

let create_table (rows: int) (cols: int) : table =
  let create_row (n: int) : piece array =
    Array.init n (fun _ -> default_piece)
  in
  { piece_matrix = Array.init rows (fun _ -> create_row cols) }

let add_piece (t: table) (pos: position) (new_piece: piece) : table =
  let row = pos.line - 1 in
  let col = pos.column - 1 in
  if row >= 0 && row < Array.length t.piece_matrix && col >= 0 && col < Array.length t.piece_matrix.(0) then
    let updated_matrix = Array.map Array.copy t.piece_matrix in
    updated_matrix.(row).(col) <- new_piece;
    { piece_matrix = updated_matrix }
  else
    t

let new_table = create_table 8 8

let new_table1 = add_piece new_table { line = 1; column = 1 } tower_piece_black
let new_table2 = add_piece new_table1 { line = 1; column = 2 } knight_piece_black
let new_table3 = add_piece new_table2 { line = 1; column = 3 } bishop_piece_black
let new_table4 = add_piece new_table3 { line = 1; column = 4 } queen_piece_black
let new_table5 = add_piece new_table4 { line = 1; column = 5 } king_piece_black
let new_table6 = add_piece new_table5 { line = 1; column = 6 } bishop_piece_black
let new_table7 = add_piece new_table6 { line = 1; column = 7 } knight_piece_black
let new_table8 = add_piece new_table7 { line = 1; column = 8 } tower_piece_black
let new_table9 = add_piece new_table8 { line = 2; column = 1 } pawn_piece_black
let new_table10 = add_piece new_table9 { line = 2; column = 2 } pawn_piece_black
let new_table11 = add_piece new_table10 { line = 2; column = 3 } pawn_piece_black
let new_table12 = add_piece new_table11 { line = 2; column = 4 } pawn_piece_black
let new_table13 = add_piece new_table12 { line = 2; column = 5 } pawn_piece_black
let new_table14 = add_piece new_table13 { line = 2; column = 6 } pawn_piece_black
let new_table15 = add_piece new_table14 { line = 2; column = 7 } pawn_piece_black
let new_table16 = add_piece new_table15 { line = 2; column = 8 } pawn_piece_black

let new_table17 = add_piece new_table16 { line = 8; column = 1 } tower_piece_white
let new_table18 = add_piece new_table17 { line = 8; column = 2 } knight_piece_white
let new_table19 = add_piece new_table18 { line = 8; column = 3 } bishop_piece_white
let new_table20 = add_piece new_table19 { line = 8; column = 4 } queen_piece_white
let new_table21 = add_piece new_table20 { line = 8; column = 5 } king_piece_white
let new_table22 = add_piece new_table21 { line = 8; column = 6 } bishop_piece_white
let new_table23 = add_piece new_table22 { line = 8; column = 7 } knight_piece_white
let new_table24 = add_piece new_table23 { line = 8; column = 8 } tower_piece_white
let new_table25 = add_piece new_table24 { line = 7; column = 1 } pawn_piece_white
let new_table26 = add_piece new_table25 { line = 7; column = 2 } pawn_piece_white
let new_table27 = add_piece new_table26 { line = 7; column = 3 } pawn_piece_white
let new_table28 = add_piece new_table27 { line = 7; column = 4 } pawn_piece_white
let new_table29 = add_piece new_table28 { line = 7; column = 5 } pawn_piece_white
let new_table30 = add_piece new_table29 { line = 7; column = 6 } pawn_piece_white
let new_table31 = add_piece new_table30 { line = 7; column = 7 } pawn_piece_white
let new_table32 = add_piece new_table31 { line = 7; column = 8 } pawn_piece_white

let piece_to_string (p : piece) : string =
  p.symbol

let str_to_piece_position str =
  let lines_pos = (8 - int_of_char str.[1] + int_of_char '0') + 1 in
  let column_pos = (int_of_char str.[0] -  int_of_char 'a') + 1 in
  { line = lines_pos; column = column_pos }
  [@@warning "-32"]

let print_table (t: table) : unit =
  Array.iteri (fun i row ->
    print_int (8 - i);
    print_string " ";
    Array.iter (fun x ->
      let str = piece_to_string x in
      print_string str; print_string " "
    ) row;
    print_newline()
  ) t.piece_matrix;
  print_string "  ";
  for i = int_of_char 'a' to int_of_char 'h' do
    let c = char_of_int i in
    print_char c;
    print_string " "
  done;
  print_newline ()
[@@warning "-32"]

let () =
  print_string "Origin: ";
  let origin = read_line () in
  print_string "Destiny: ";
  let destiny = read_line () in
  let c_origin = str_to_piece_position origin in
  let c_destiny = str_to_piece_position destiny in
  Printf.printf "Origin: %d-%d\nDestiny: %d-%d\n" c_origin.line c_origin.column c_destiny.line c_destiny.column;
  print_table new_table32