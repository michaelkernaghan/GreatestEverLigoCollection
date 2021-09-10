(* # let add x y = x + y;;

  # let f = add 6;;

  # f 5;;
*)

(* recall: *)

let rec map f l =
  match l with
    [] -> []
    | h::t -> f h :: map f t;;

let rec mapl f l =
  match l with[] -> []
  | h::t -> map f h :: mapl f t;;

(* with partial application mapl can be written as *)

let mapl f l = map (map f) l;;

(* the partially applied function map f is of type a list -> b list, which i sexactly the right
 type topass to map when mapping over lists of lists. In fact: *)
 let mapl f = map (map f)

 (* Here, map (map f) has type a list list -> b list list so when f is applied to mapl, a function is returned
 requiring just the list. This is partial application at work. *)
  
 (* QUESTIONS *)
 (* 2. Recall the function member x l which determines if an element x is contained in a list l. What is its
       type? What is the type of member x? Use partial application to write a function member_all x ls 
       which determines if an element is a memeber of all the lists in the list of lists ls 
  *)

        let rec member e l =
          match l with  
            [] -> false
            | h::t -> h = e || member e t;;

(* The type of member is a -> list -> bool, so if we partially applly the fist argument, the type of member
  x must be a list -> bool *)

let member_all x ls =
  let booleans = map (member x) ls in
    not (member false booleans);;

    (* or *)
let member_all x ls = 
  not (member false (map (member x) ls));;

(* why check for the abscence of false rather than the presence of true? *)

(* 3. Why can we not write a function to halve all the elements of a list like this: 
    map (( / ) 2) [10; 20; 30]
  ? Write a suitable division function which can be partially applied in the manner we require.
*) 
  let rec map f l =
    match l with
      [] -> []
      | h::t -> f h :: map f t;;

  let rdiv x y = y / x;; 
  map ((rdiv) 2) [10; 20; 30];;

(* 4. Write a function mapll which maps a function over lists of lists of lists. You must not use the let
 rec construct. Is it possible to write a function which works like map, mapl, or mapll depending upon
 the list given to it?
*)

let mapll f l = map ( map ( map f)) l;;

let mapll f = map ( map ( map f));;

(* 5. Write a function truncate which takes an integer and a list of lists, and returns a list of lists, each 
  of which has been truncated to a given length. If a list is shorter than the given length, it is
  unchanged. Make use of partial application. 
*)

let rec truncate i ll = 
  