(* Write a contract in CameLigo that demonstrates updating big maps *)

(* initial storage *)
(* {values=Big_map.literal[(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), 1)]} *)

(* Deployed to Granadanet as: KT18qMkAbUn5EZBxdTYqzRzTfjS9FHr6Lfjm *)

type value = int 

type register = (address, value) big_map

type storage = { values : register }

type returnType  = operation list * storage

let values : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 1);
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), 3)]

let main (_p, s : unit * storage) : returnType =
      let updated_map: register =
      Big_map.update Tezos.sender (Some 4) s.values in
    let s = {s with values = updated_map}
  in ([] : operation list), s  


