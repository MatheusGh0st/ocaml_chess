type position = { line: int; column: int } [@@warning "-69"]

type piece_color = Black | White | None [@@warning "-37"]

type piece = { position: position; color: piece_color; symbol: string } [@@warning "-69"]

type table = { piece_matrix: piece array array } [@@warning "-69"]

type player_color = Black | White [@@warning "-37"]

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
  { position = { line = 0; column = 0}; color = Black; symbol = "P"}

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

let remove_piece (t: table) (pos: position) ?(new_piece: piece option) : table =
  let row = pos.line - 1 in
  let col = pos.column - 1 in
  if row >= 0 && row < Array.length t.piece_matrix &&
      col >= 0 && col < Array.length t.piece_matrix.(0) &&
      t.piece_matrix.(row).(col).symbol <> "-" then
    let updated_matrix = Array.map Array.copy t.piece_matrix in
    let save_piece = t.piece_matrix.(row).(col) in
    let updated_piece =
      match new_piece with
      | Some(piece) -> piece
      | None -> save_piece
    in
    updated_matrix.(row).(col) <- updated_piece;
    { piece_matrix = updated_matrix }
  else
    t
  [@@warning "-16"]

let move_piece (t: table) (source_pos: position) (dest_pos: position) ?(new_piece: piece option) (mv_array: piece array) : table * piece array =
  let src_row = source_pos.line - 1 in
  let src_col = source_pos.column - 1 in
  let dest_row = dest_pos.line - 1 in
  let dest_col = dest_pos.column - 1 in
  if src_row >= 0 && src_row < Array.length t.piece_matrix &&
    src_col >= 0 && src_col < Array.length t.piece_matrix.(0) &&
    dest_row >= 0 && dest_row < Array.length t.piece_matrix &&
    dest_col >= 0 && dest_col < Array.length t.piece_matrix.(0) then
      let updated_matrix = Array.map Array.copy t.piece_matrix in
      let moved_piece = updated_matrix.(src_row).(src_col) in
      let captured_piece = updated_matrix.(dest_row).(dest_col) in

      updated_matrix.(src_row).(src_col) <- default_piece;

      if captured_piece.symbol <> "-" then
        updated_matrix.(dest_row).(dest_col) <- moved_piece;

      let updated_mv_array =
        match new_piece with
        | Some(piece) -> Array.append mv_array [|moved_piece; piece|]
        | None -> Array.append mv_array [|moved_piece|]
      in

      let updated_table =
        match new_piece with
        | Some(piece) -> add_piece { piece_matrix = updated_matrix } dest_pos piece
        | None -> add_piece { piece_matrix = updated_matrix } dest_pos moved_piece
      in

      let final_table, final_mv_array =
        if captured_piece.symbol <> "-" then
          let updated = remove_piece updated_table dest_pos ?new_piece:None in
          updated, Array.append updated_mv_array [|captured_piece|]
        else
          updated_table, updated_mv_array
      in

      final_table, final_mv_array
    else
      t, mv_array
[@@warning "-32"]


let valid_pawn_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = source_pos.line - dest_pos.line in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  let valid_first_move =
    if t_src.symbol = "P" then
      match t_src.color with
      | White -> source_pos.line = 2 && row_diff = 2 && col_diff = 0 && t_des.symbol = "-"
      | Black -> source_pos.line = 7 && row_diff = -2 && col_diff = 0 && t_des.symbol = "-"
      | _ -> false
    else false
  in

  let valid_forward_move =
    if t_src.symbol = "P" then
      match t_src.color with
      | White -> row_diff = 1 && col_diff = 0 && t_des.symbol = "-"
      | Black -> row_diff = -1 && col_diff = 0 && t_des.symbol = "-"
      | _ -> false
    else false
  in

  let valid_capture =
    row_diff = 1 && col_diff = 1 && t_des.symbol <> "-" &&
    t_des.color <> t_src.color
  in

  valid_first_move || valid_forward_move || valid_capture

let convert_player_to_piece_color (c: player_color) : piece_color =
  match c with
  | Black -> Black
  | White -> White

let is_your_turn (t: table) (source_pos: position) (c_color: player_color) : bool =
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in

  let con_color = convert_player_to_piece_color c_color in
  let same_color = 
    t_src.color = con_color in
  
  same_color && t_src.symbol <> "-"

let valid_king_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = abs (source_pos.line - dest_pos.line) in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  (row_diff <= 1 && col_diff <= 1) &&
  (t_src.symbol = "K" && t_des.symbol <> "K" && t_des.color <> t_src.color)

let valid_tower_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = abs (source_pos.line - dest_pos.line) in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  (row_diff = 0 || col_diff = 0) &&
  not (row_diff = 0 && col_diff = 0) &&
  t_src.symbol = "T" &&
  t_src.color <> t_des.color
  
let valid_knight_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = abs (source_pos.line - dest_pos.line) in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  let valid_l_shape =
    (row_diff = 1 && col_diff = 2) || (row_diff = 2 && col_diff = 1)
  in
  let not_staying = not (row_diff = 0 && col_diff = 0) in
  let is_knight = t_src.symbol = "N" in
  let can_move = t_des.symbol = "-" || t_des.color <> t_src.color in

  valid_l_shape && not_staying && is_knight && can_move
  
let valid_queen_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = abs (source_pos.line - dest_pos.line) in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  let is_valid_horizontal_or_vertical =
    (row_diff = 0 || col_diff = 0) && not (row_diff = 0 && col_diff = 0)
  in
  let is_valid_diagonal =
    row_diff = col_diff && not (row_diff = 0 && col_diff = 0)
  in
  let is_queen = t_src.symbol = "Q" in
  let can_move = t_des.symbol = "-" || t_des.color <> t_src.color in

  (is_valid_horizontal_or_vertical || is_valid_diagonal) &&
  is_queen && can_move
  
let valid_empty_path t source_pos dest_pos =
  let rec check_path t (current_pos : position) (dest_pos : position) : bool =
    if current_pos = dest_pos then
      true
    else
      let row_step = if dest_pos.line > current_pos.line then 1 else -1 in
      let col_step = if dest_pos.column > current_pos.column then 1 else -1 in
      let next_pos = { line = current_pos.line + row_step; column = current_pos.column + col_step } in
      let piece_at_next_pos = t.piece_matrix.(next_pos.line - 1).(next_pos.column - 1) in
      if piece_at_next_pos.symbol <> "-" then
        false
      else
        check_path t next_pos dest_pos
  in
  let row_diff = dest_pos.line - source_pos.line in
  let col_diff = dest_pos.column - source_pos.column in
  if abs row_diff <> abs col_diff then
    false
  else
    check_path t source_pos dest_pos

let valid_bishop_move (t: table) (source_pos: position) (dest_pos: position) : bool =
  let row_diff = abs (source_pos.line - dest_pos.line) in
  let col_diff = abs (source_pos.column - dest_pos.column) in
  let t_src = t.piece_matrix.(source_pos.line - 1).(source_pos.column - 1) in
  let t_des = t.piece_matrix.(dest_pos.line - 1).(dest_pos.column - 1) in

  let same_color_diagonal =
    row_diff = col_diff
  in
  let not_staying = not (row_diff = 0 && col_diff = 0) in

  let can_capture =
    t_des.symbol <> "-" && t_src.color <> t_des.color
  in

  same_color_diagonal && not_staying && (t_des.symbol = "-" || can_capture) && valid_empty_path t source_pos dest_pos

exception KingNotFound

let find_king_position (t: table) (color: piece_color) : position =
  let rec find_king t row col =
    if row >= Array.length t.piece_matrix then
      raise KingNotFound
    else if col >= Array.length t.piece_matrix.(0) then
      find_king t (row + 1) 0
    else
      let piece = t.piece_matrix.(row).(col) in
      if piece.color = color && piece.symbol = "K" then
        { line = row + 1; column = col + 1 }
      else
        find_king t row (col + 1)
  in
  find_king t 0 0

let is_position_under_threat (t: table) (pos: position) (color: piece_color) : bool =
  let rec check_positions positions =
    match positions with
    | [] -> false
    | hd :: tl ->
        let piece = t.piece_matrix.(hd.line - 1).(hd.column - 1) in
        if piece.color <> color then
          match piece.symbol with
          | "P" -> valid_pawn_move t hd pos
          | "K" -> valid_king_move t hd pos
          | "Q" -> valid_queen_move t hd pos
          | "B" -> valid_bishop_move t hd pos
          | "N" -> valid_knight_move t hd pos
          | "T" -> valid_tower_move t hd pos
          | _ -> false (* Invalid piece *)
        else
          check_positions tl
  in

  let row_numbers = List.init 8 (fun x -> x + 1) in
  let column_numbers = List.init 8 (fun x -> x + 1) in
  let positions = List.concat (List.map (fun row -> List.map (fun col -> { line = row; column = col }) column_numbers) row_numbers) in
  check_positions positions

let is_check (t: table) (color: piece_color) : bool =
  try
    let king_pos = find_king_position t color in
    is_position_under_threat t king_pos color
  with
  | KingNotFound -> false

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

let moves_pieces_array: piece array = [|default_piece|]

let piece_to_string (p : piece) : string =
  p.symbol

let str_to_piece_position str =
  let lines_pos = (8 - int_of_char str.[1] + int_of_char '0') + 1 in
  let column_pos = (int_of_char str.[0] -  int_of_char 'a') + 1 in
  { line = lines_pos; column = column_pos }
  [@@warning "-32"]

let is_valid_move (t: table) (src_pos: position) (dest_pos: position) : bool =
  let piece = t.piece_matrix.(src_pos.line - 1).(src_pos.column - 1).symbol in
  let is_valid =
    match piece with
    | "P" -> valid_pawn_move t src_pos dest_pos
    | "K" -> valid_king_move t src_pos dest_pos
    | "Q" -> valid_queen_move t src_pos dest_pos
    | "B" -> valid_bishop_move t src_pos dest_pos
    | "N" -> valid_knight_move t src_pos dest_pos
    | "T" -> valid_tower_move t src_pos dest_pos
    | _ -> false
  in
  is_valid

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

let is_input_valid o_input d_input =
  let is_valid =
    (String.length o_input > 2 || String.length d_input > 2
    || String.length o_input = 0 || String.length d_input = 0)
  in
  is_valid

let init_player = White

let change_player (color: player_color): player_color =
  let update_player = match color with
  | White -> Black
  | Black -> White
  in
  update_player

let color_to_string color =
  let convert_color = match color with
  | White -> "White"
  | Black -> "Black"
  in
  convert_color

let rec program table color =
  let _ = Sys.command "clear" in
  Printf.printf "Awaiting the color: %s\n" (color_to_string color);
  print_table table;
  print_string "Origin: ";
  let origin = read_line () in
  print_string "Destiny: ";
  let destiny = read_line () in
  let c_origin = str_to_piece_position origin in
  let c_destiny = str_to_piece_position destiny in
  if (is_check table (convert_player_to_piece_color color)) then begin
    Printf.printf "Player in check\n";
    program table color
  end else
    if (is_input_valid origin destiny)
      || not (is_valid_move table c_origin c_destiny) || not (is_your_turn table c_origin color) then begin
      Printf.printf "Please enter a valid position\n";
      program table color
    end else
      let update_table table o_pos d_pos a_pieces =
        let mv_table = move_piece table o_pos d_pos a_pieces in
        let up_player = change_player color in
        program (fst mv_table) up_player
      in
      update_table table c_origin c_destiny moves_pieces_array
  
let () = program new_table32 init_player