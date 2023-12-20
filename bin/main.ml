type position = { line: int; column: int } [@@warning "-69"]

type piece_color = Black | White | None [@@warning "-37"]

type piece = { position: position; color: piece_color; symbol: string } [@@warning "-69"]

type table = { piece_matrix: piece array array }

let default_position = { line = 0; column = 0}

let default_piece = { position = default_position; color = None; symbol = "-" }

let create_table (rows: int) (cols: int) : table =
  let create_row (n: int) : piece array =
    Array.init n (fun _ -> default_piece)
  in
  { piece_matrix = Array.init rows (fun _ -> create_row cols) }

let piece_to_string (p : piece) : string =
  p.symbol

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

let table1 = create_table 8 8

let () =
  print_table table1