let rec insert x l = 
  match l with
  [] -> [x]
  | h::t ->
    if x <= h
      then x :: h :: t
      else h :: insert x t;;

      let rec sort l = 
        match l with
        [] -> []
        | h::t -> insert h (sort t);;

(* In msort, we calculate the value of the expression 1/2 twice. 
Modify msort to remove this inefficiency *)

let rec take n l =
  if n = 0 then [] else
      match l with
      h::t -> h :: take (n - 1) t;;

let rec drop n l =
  if n = 0 then l else
    match l with 
    [] -> []
    | h::t -> drop (n-1) t;;

let rec length l =
  match l with
  [] -> 0
  | h::t -> 1 + length t;;

  let rec merge x y =
    match x, y with
    [], l -> l
    | l, [] -> l
    | hx::tx, hy::ty ->
      if hx < hy then hx :: merge tx (hy :: ty)
      else hy :: merge (hx :: tx) ty;;
let rec msort l =
  match l with 
  [] -> []
  | [x] -> [x]
  |_ ->
    let x = (length l/2) in
    let left = take x l in
      let right = drop x l in
        merge (msort left) (msort right);;

(* Write a version of insertion sort which sorts the argument list into reverse order *)

let rec insert x l = 
  match l with
  [] -> [x]
  | h::t ->
    if x >= h
      then x :: h :: t
      else h :: insert x t;;

      let rec sort l = 
        match l with
        [] -> []
        | h::t -> insert h (sort t);;

(* Write a function to detect if a list is already in sorted order *)

  let rec is_sorted l =
    match l with
    [] -> true
    | [x] -> true
    | a::b::t -> a <= b && is_sorted (b::t);;
  
  (* reverse the cases *)
  let rec is_sorted l =
    match l with
    a::b::t -> a <= b && is_sorted (b::t)
    | _ -> true;;

  (* Observe how char list is sorted *)
  (* 
       # sort [['o'; 'n'; 'e']; ['t'; 'w'; 'o']; ['t'; 'h'; 'r'; 'e'; 'e']];;
       - : char list list =
       [['o'; 'n'; 'e']; ['t'; 'h'; 'r'; 'e'; 'e']; ['t'; 'w'; 'o']]
  *)

  (* Combine the sort and insert functions into a single sort funtion *)

  let rec sort l =
    let rec insert x s =
    match s with
    [] -> [x]
    | h::t ->
      if x <= h
        then x :: h :: t
    else h :: insert x t
  in
      match l with
      [] -> []
      | h::t -> insert h (sort t);;