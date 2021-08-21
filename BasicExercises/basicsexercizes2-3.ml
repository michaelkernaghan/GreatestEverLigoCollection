let not x = 
if x then false else true;;

let not x =
    match x with
    x -> false
    |_ -> true ;;

let rec sum n =
  if n = 1 then 1 else n + sum (n - 1)

let rec sum n =
    match n with
    1 -> 1
    | n -> n + sum (n - 1)

let rec power x n =
  if n = 0 then 1 else
    if n = 1 then x else
      x * power x (n - 1)

let rec power x n =
    match n with
     0 -> 1
    |1 -> x
    |_ ->  x * power x (n - 1)

match 1 + 1 with 
    2 -> match 2 + 2 with 
        3 -> 4
        |4 -> 5

let islower c =
    match c with
    'a'..'z' -> true
    | _ -> false;;

let isupper c =
    match c with
    'A'..'Z' -> true
    | _ -> false;;