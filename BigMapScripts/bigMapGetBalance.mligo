(* Write a contract in CameLigo that demonstrates updating big maps and getting balance *)

(* initial storage *)
(* Big_map.literal[(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), 100tez)] *)

type storage = (address, tez) big_map

type action =
  Update
| GetBalance

let update (s : storage) :  storage =
  Big_map.update Tezos.sender (Some Tezos.amount) s 

let get_balance (s : storage) : storage =
  let value = match Big_map.find_opt Tezos.sender s with
    Some value -> value
    | None -> 0tez
  in s

let main (action, storage : action * storage) : operation list * storage =  
 ([]: operation list),
 (match action with
 | Update -> update (storage)
 | GetBalance -> get_balance (storage)
 )