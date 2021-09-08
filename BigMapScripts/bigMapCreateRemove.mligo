type move =  int * int

type register =  (address, move) big_map

type storage =  { registers : register }

type return =  operation list * storage

let moves : register =
  Big_map.literal
    [(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address),
     (1, 2))]

type action =  CreateBigMap | RemoveBigMap

let create_bigmap (_p : register) : register =
  Big_map.literal
    [(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address),
     (1, 2))]

let remove_bigmap (_p : register) : register =
  Big_map.remove
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)
    moves

let main (action, registers : action * register) :
  (operation list * register) =
  ([] : operation list),
  (match action with
    CreateBigMap _r -> create_bigmap registers
  | RemoveBigMap _r -> remove_bigmap registers)
