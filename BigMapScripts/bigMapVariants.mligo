(* Write a contract in CameLigo that demonstrates updating big maps and getting balance *)

(* initial storage *)
(* {compound_keys = Big_map.literal [((("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), 1n), 10n)]};
{compounds = Big_map.literal[(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), ((100tez),
("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address)))]};
{simples = Big_map.literal[(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address), 100tez)]}
 *)

type stat = {
    sender : address;
    score : int
}

type token_id = nat

type owner = address

(* (owner,token_id) -> balance *)
type compound_key = ((address * token_id), nat) big_map

type simple = (address, tez) big_map

type compound =  (address, stat) big_map

type storage = { 
    simples : simple;
    compounds : compound;
    compound_keys : compound_key
  }

type parameter =
  SingleValue of tez
| CompoundValue of int
| CompoundKey of nat

type return = operation list * storage

let update_simple_value (p, s : tez * storage) : storage =
  { s with simples = Big_map.update Tezos.sender (Some p) s.simples }

let update_compound_value (p, s : int * storage) : storage = 
  let stat : stat = { sender = Tezos.sender; score = p } in
  { s with compounds = Big_map.update Tezos.sender (Some stat) s.compounds }

let update_compound_key (token_id, s : nat * storage) : storage =
    let key = Tezos.sender, token_id in
    {s  with compound_keys = Big_map.update key (Some 10n) s.compound_keys }

let main (p, s : parameter * storage) : operation list * storage =  
 ([]: operation list),
 (match p with
 | SingleValue n -> update_simple_value (n, s)
 | CompoundValue n -> update_compound_value (n, s)
 | CompoundKey n -> update_compound_key (n, s)
 )