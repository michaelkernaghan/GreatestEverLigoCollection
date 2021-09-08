
(* example *)
let rec last l =
  match l with
  [x] -> x
  | _::t -> last t;;

(* gets Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
val last : 'a list -> 'a = <fun> *)


let rec last l =
  match l with
  [] -> raise Not_found
  | [x] -> x
  | _::t -> last t;;



(* Write a function smallest which returns the smallest positive elements of a list of integers. If there
 is no positive element, it should raise the built-in NOT_found exception.*)

 (* let rec smallest l =
  match l with
  [] -> []
  | h::t ->  if h < 1 raise Not_found && smallest t
  else if      *)

  let rec smallest_inner current found l =
    match l with
    [] ->
      if found then current else raise Not_found
    | h::t -> 
      if h > 0 && h < current 
        then smallest_inner h true t
        else smallest_inner current found t;;

  let smallest l =
    smallest_inner  max_int false l;;

    (* Write another function smallest_or_zero which uses the smallest function but if Not_found is 
      raised, returns zero *)

    let smallest_or_zero l =
      try smallest l with Not_found -> 0

(* Write a function sqrt_inner which, given a test number x and a target number n, squares and tests if 
  it is more than n. *)

let rec sqrt_inner x n =
  if x * x > n then x - 1 else sqrt_inner (x + 1) n;;

exception Complex;;
let sqrt n =
  if n < 0 then raise Complex else sqrt_inner 1 n;;

  let safe_sqrt n =
  try sqrt n with Complex -> 0;;

(* Write an exception definition and a function which calculates the largest integer smaller than or
equal to the square root of a given integer. If the argument is negative, the exception should be 
raised *)

exception Negative_argument;;

let rec sqrt_int x = 
  if x < 0 then raise Negative_argument
      sqrt x
