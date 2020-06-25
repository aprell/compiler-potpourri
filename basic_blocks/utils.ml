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

(*$T take
  take 0 [] = []
  take 1 [1; 2; 3] = [1]
  take 2 [1; 2; 3] = [1; 2]
  take 3 [1; 2; 3] = [1; 2; 3]
  take 4 [1; 2; 3] = [1; 2; 3]
*)
let take n lst =
  let rec loop i acc = function
    | [] -> acc
    | hd :: tl ->
      if i > 0 then loop (i - 1) (hd :: acc) tl else acc
  in
  loop n [] lst
  |> List.rev

(*$T drop
  drop 0 [] = []
  drop 1 [1; 2; 3] = [2; 3]
  drop 2 [1; 2; 3] = [3]
  drop 3 [1; 2; 3] = []
  drop 4 [1; 2; 3] = []
*)
let rec drop n = function
  | [] -> []
  | _ :: tl as lst ->
    if n > 0 then drop (n - 1) tl else lst

(*$T split
  split 0 [] = ([], [])
  split 0 [1; 2; 3] = ([], [1; 2; 3])
  split 1 [1; 2; 3] = ([1], [2; 3])
  split 2 [1; 2; 3] = ([1; 2], [3])
  split 3 [1; 2; 3] = ([1; 2; 3], [])
  split 4 [1; 2; 3] = ([1; 2; 3], [])
*)
let split i lst = (take i lst, drop i lst)

(*$T sublist
  sublist 0 1 [] = []
  sublist 0 1 [1; 2; 3; 4; 5] = [1]
  sublist 0 2 [1; 2; 3; 4; 5] = [1; 2]
  sublist 2 2 [1; 2; 3; 4; 5] = []
  sublist 3 2 [1; 2; 3; 4; 5] = []
  sublist 3 5 [1; 2; 3; 4; 5] = [4; 5]
  sublist 0 6 [1; 2; 3; 4; 5] = [1; 2; 3; 4; 5]
*)
let sublist i j lst = take (j - i) (drop i lst)

(*$T pad
  pad "hi" 2 = "hi"
  pad "hi" 3 = "hi "
  pad "hi" 4 = "hi  "
  pad "hi" 5 = "hi   "
*)
let pad str len =
  str ^ String.make (len - String.length str) ' '

let print_table ~column_widths ~rows =
  let hline = List.map (Fun.flip String.make '-') column_widths in
  let print_hline () =
    hline
    |> String.concat "-+-"
    |> Printf.sprintf "+-%s-+"
    |> print_endline
  in
  let print_row row =
    List.map2 pad row column_widths
    |> String.concat " | "
    |> Printf.sprintf "| %s |"
    |> print_endline
  in
  print_hline ();
  print_row (List.hd rows);
  print_hline ();
  List.iter print_row (List.tl rows);
  print_hline ()
