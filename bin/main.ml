type position = { line: int; column: int } [@@warning "-69"]

type piece_color = Black | White | None [@@warning "-37"]

type piece = { position: position; symbol: string } [@@warning "-69"]

let default_position = { line = 0; column = 0}

let default_piece = { position = default_position; color = None; symbol = "-" }

let create_table (rows: int) (cols: int) : table =
  let create_row (n: int) : piece array =
    Array.init n (fun _ -> default_piece)
  in
  { piece_matrix = Array.init rows (fun _ -> create_row cols) }

  