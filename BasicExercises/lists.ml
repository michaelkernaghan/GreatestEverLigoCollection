let rec odd_elements l =
    match l with  [] -> []
    | [a] -> [a]
    | a::_::t -> a :: odd_elements t;;

let rec even_elements l =
  match l with  [] -> []
  | [a] -> [a]
  | a::_::t -> a :: even_elements t;;

  let rec even_elements l =
    match l with
      [] -> []
    | [_] -> []
    | _::b::t -> b :: even_elements t;;

  let rec count_true_inner n l =
    match l with
    [] -> n
    | true::t -> count_true_inner (n+1) t
    | false::t -> count_true_inner n t;;

  let count_true l =
    count_true_inner 0 l;;

    (* Write a function which, given a list, will contruct a palindrome from it. *)
    (* A palindrome is a list which equals its own reverse.*)
    (* You can assume the existence of rev and @. Write another function that checks if a list is a palindrome *)

      (* need rev from the example *)
      let rec rev l =
        match l with 
        [] -> []
        | h::t -> rev t @ [h];;
      let rec make_palindrome l =
        match l with
          [] -> []
        | h::t -> rev t @ [h] @ t ;;

        (* answer *)
        let mk_palindrome l =
          l @ rev l;;
        let check_palindrome l = 
          l = rev l;;

   (* check_palindrome (mk_palindrome [1;2;3;4]) *)

   (* Write a function drop_last which returns all but the last element of a list. If the 
   list is empty, it should return the empty list. so, for example, drop_last [1; 2; 4; 8] should return [1; 2; 4]
    What about a recursive tail version? *)
    let rec drop n l =
      if n = 0 then l else
        match l with 
        [] -> []
        | h::t -> drop (n-1) t;;

    let drop_last l =
      match l with 
      [] -> []
      | l -> drop 1 l;;

      (* tail recursive version *)
      let rec drop_last_inner a l =
        match l with
          [] -> rev a
          | [_] -> rev a
          | h::t -> drop_last_inner ( h::a) t;;
      let drop_last l =
        drop_last_inner [];;

(* Write a function MEMBER of type a -> a list -> bool which returns true if is an element exisys in a list,
or false if not. For example, member 2 [1; 2; 3] should evaluate to true, but member 3 [1; 2] should evaluate to false *)

let rec member e l =
  match l with  
    [] -> false
    | h::t -> h = e || member e t;;

(* Use your member function to write a function mkae_set which, given a list, resturns a list which 
contains all the elemets of the rginal list, but ha no duplicate elements. FOr example, make_set 
[1; 2; 3; 3; 1 ] might return [2; 3; 1]. What is the type of your function*)

let rec member e l =
  match l with  
    [] -> false
    | h::t -> h = e || member e t;;