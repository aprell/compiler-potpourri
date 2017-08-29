let input_lines chan =
  let rec loop lines =
    match input_line chan with
    | line -> loop (line :: lines)
    | exception End_of_file -> List.rev lines
  in
  loop []

let read_file name =
  let file = open_in name in
  let lines = input_lines file in
  close_in file;
  lines

(*$T ( -- )
   0 --  0 = [0]
   0 --  1 = [0; 1]
   0 --  2 = [0; 1; 2]
  -2 --  0 = [-2; -1; 0]
  -2 -- -1 = [-2; -1]
  -2 -- -2 = [-2]
  -2 -- -3 = []
*)
let ( -- ) i j =
  let rec loop i l =
    if i > j then l
    else loop (i + 1) (i :: l)
  in
  loop i []
  |> List.rev

(*$T ( ^^ )
  "x" ^^ [] = []
  "x" ^^ [1] = ["x1"]
  "x" ^^ [1; 2] = ["x1"; "x2"]
  "x" ^^ [1; 2; 3] = ["x1"; "x2"; "x3"]
*)
let ( ^^ ) pref lst =
  List.map (fun n -> pref ^ string_of_int n) lst
