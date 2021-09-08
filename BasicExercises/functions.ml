let rec double l = 
  match l with
  [] -> []
  | h::t -> (h  * 2) :: double t;;

  let rec evens l =
    match l with 
    [] -> []
    | h::t -> (h mod 2 = 0) :: evens t;;

  let rec map f l =
    match l with
      [] -> []
      | h::t -> f h :: map f t;;

let have x = x / 2;;

(* Write a simple recursive function calm to replace excalmation marks in a char list with periods. for
example calm ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'] should evaluate to 
('H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.']. 

Now rewrite your function to use map instead of recursion. What are the types of your functions? *)

let rec calm l =
  match l with
  [] -> []
  | '!'::t -> '.' :: calm t
  | h::t ->  h:: calm t ;;

let rec map f l =
  match l with
  [] -> []
  | h::t -> f h :: map f t
let calm_char x =
  match x with '!' -> '.' | _ -> x;;

  let calm l =
    map calm_char l
  ;;

  (* Write a function clip which, given an integer, clips it to the range 1...10 so that integers bigger
  than 10 round down to 10, and those smaller than 1 round up to 1. Write another function cliplist 
  that uses this first function together with map to apply this clipping to a whole list of integers *)

  let clip x =
    if x < 1 then 1 else
     if x > 10 then 10 else x ;;

    let cliplist l =
      map clip l;;

(* Express your function cliplist again, this time using an anonymous function instead of clip. *)

let cliplist l =
  map (fun x -> 
        if x < 1 then 1 else
          if x > 10 then 10 else x) 
                l;;

(* Write a function apply which, given any other function, a nunber of times to apply it, and an initial
argument for the function, will return the cumulative effect of repeatedly applying the function. For 
instance, apply f 6 4 would return f (f (f (f (f (f 4))))))). What is the type of your function? *)
let rec apply f n x =
  if n = 0
    then x
    else f (apply f (n - 1) x) ;;

(* Modify the insertion sort function from the preceding chapter to take a comparison function, in the
 same way that we modified merge sort in this chapter. What is its type? *)

let less a b =
  a <= b

 let rec insert less x l = 
  match l with
  [] -> [x]
  | h::t ->
    if less x h
      then x :: h :: t
      else h :: insert less x t;;

      let rec sort less l = 
        match l with
        [] -> []
        | h::t -> insert less h (sort less t);;

(* Write a function filter which takes a function of type a -> bool and an a list and returns a list of 
just those elements of the argument list for which the given function returns true. *)

let rec filter f l =
  match l with
  [] -> []
  | h::t -> if f h 
        then h :: filter f t
        else filter f t;;

(* Write the function for_all which, given a function of type a -> bool and an argument list of type
a list evaluates to true if and only if the function returns true for every element of the list. Give
  examples of its use. *)

let rec for_all f l =
  match l with
  [] -> true
  | h::t -> f h && for_all f t;;

(* Write a function mapl which maps a function of type a -> b over a list of type a list list to produce 
a list of type B list list*)

let rec mapl f l =
  match l with
  [] -> []
  | h::t -> map f h :: mapl f t;;






