let p = (1, 4);;
let q = ( 1, '1');;

let fst p = match p with (x, _) -> x;;
let snd p = match p with (_, y) -> y;;

(* let fst (x, _) = x
let snd (_, y) = y *)

(* (int x int) list *)
(* let census = [(1, 4); (2,2); (3, 2); (4, 3); (5, 1); (6, 2)];; *)

(* let y = (1, [2; 3; 4]) *)

let rec lookup x l = 
  match l with
    [] -> raise Not_found
    | (k, v)::t ->
      if k = x then v else lookup x t;;

(* lookup 4 census *)

let rec add k v d = 
  match d with
  [] -> [(k, v)]
  | (k', v')::t -> 
      if k = k' 
        then (k, v) :: t
        else  (k', v') :: add k v t;;

(*  add 6 2 [(4, 5); (6, 3)];; *)

let rec remove k d = 
  match d with
    [] -> []
    | (k', v')::t ->
      if k = k' 
        then t
        else (k', v') :: remove k t;;

(*  remove 6 [(4, 5); (6, 3)];; *)

let key_exists k d = 
  try 
    let _ = lookup k d in true
with
  Not_found -> false

(* Write a function to determine the number of different keys in a dictionary *)

let rec key_count d =
  match d with
  [] -> 0
  |h::t -> 1 + key_count t;;


(* Define a function replace which is like add, but raises Not_found if the key is not already there*)

  let rec replace k v l = 
    match l with
    [] -> raise Not_found
    | (k', v')::t -> 
        if k = k' 
          then (k, v) :: t
          else  (k', v') :: replace k v t;;

(* Write a function to build a dictionary from two equal length lists, one containing keys and another
 containing values. Raise the exception Invalid_argument if the lists are not of equal length *)

  let rec mkdict keys values =
    match keys, values with
    [], [] -> []
    | [], _ -> raise (Invalid_argument "mkdict")
    | _, [] -> raise (Invalid_argument "mkdict")
    | k::ks, v::vs -> (k, v) :: mkdict ks vs;;

    (* Now write the inverse function: given a dictionary, return the pair of two lists - the first containing
     all the keys , and the second containing all the values *)

   let rec mklists l =
    match l with
    [] -> [], []
    | (k, v)::more ->
      match mklists more with
       (ks, vs) -> (k :: ks, v :: vs);;

      (* mklists [(1, 4); (2,2); (3, 2); (4, 3); (5, 1); (6, 2)];; *)

  let rec mklists l =
  match l with
  [] -> [], []
  | (k, v)::more ->
    let (ks, vs) = mklists more in
      (k :: ks, v :: vs);;

(* Define a function to turn any list of pairs into a dictionary. If duplicate keys are found, the value
 associated with the first occurrence of the key should be kept *)

 let rec dictionary_of_pairs_inner keys_seen l =
  match l with
  [] -> []
  |(k, v)::t -> 
    if member k keys_seen
      then dictionary_of_pairs_inner keys_seen t
      else (k, v) :: dictionary_of_pairs_inner (k :: keys_seen) t;;
let dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l

   (* dictionary_of_pairs [(1, 4); (2,2); (3, 2); (4, 3); (4, 2); (5, 1); (6, 2)];; *)

(* Write the function union a b which forms the union of two dictionaries. The union of two dictio-
    naries is the dictionary containing all the entries in one or other or both. In the case that a key is
    contained in both dictionaries, the value in the first should be preferred.*)

let rec union a b =
  match a with
  [] -> b
  | (k, v)::t -> add k v (union t b)

(* union [(1, 4); (2,2); (3, 2)] [(4, 3); (5, 1); (3, 2)];; *)