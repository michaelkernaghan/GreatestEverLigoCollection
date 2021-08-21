let rec odd_elements l =
    match l with  [] -> []
    | [a] -> [a]
    | a::_::t -> a :: odd_elements t

let rec even_elements l =
  match l with  [] -> []
  | [a] -> [a]
  | a::_::t -> a :: even_elements t

  let rec even_elements l =
    match l with
      [] -> []
    | [_] -> []
    | _::b::t -> b :: even_elements t