type stat = {sender : address; score : nat}

type token_id = nat

type owner = address

type compound_key = ((address * token_id), nat) big_map

type simple_value = (address, tez) big_map

type compound_value = (address, stat) big_map

type compound_key_compound_value =
  (address * token_id, stat) big_map

type storage =
  {simple_values : simple_value;
   compound_values : compound_value;
   compound_keys : compound_key;
   compound_keys_compound_values :
     compound_key_compound_value}

type parameter =
  SingleValue of tez | CompoundValue of nat
| CompoundKey of nat | CompoundKeyCompoundValue of nat

type return = operation list * storage

let update_simple_value (p, s : tez * storage) : storage =
  {s with
    simple_values =
      Big_map.update Tezos.sender (Some p) s.simple_values}

let update_compound_value (p, s : nat * storage) : storage =
  let stat : stat = {sender = Tezos.sender; score = p} in
  {s with
    compound_values =
      Big_map.update
        Tezos.sender
        (Some stat)
        s.compound_values}

let update_compound_key (token_id, s : nat * storage)
: storage =
  let key = Tezos.sender, token_id in
  {s with
    compound_keys =
      Big_map.update key (Some token_id) s.compound_keys}

let update_compound_key_compound_value
  (p, s : nat * storage) : storage =
  let stat : stat = {sender = Tezos.sender; score = p} in
  {s with
    compound_keys_compound_values =
      Big_map.update
        (sender, p)
        (Some stat)
        s.compound_keys_compound_values}

let main (p, s : parameter * storage)
: operation list * storage =
  ([] : operation list),
  (match p with
     SingleValue n -> update_simple_value (n, s)
   | CompoundValue n -> update_compound_value (n, s)
   | CompoundKey n -> update_compound_key (n, s)
   | CompoundKeyCompoundValue n ->
       update_compound_key_compound_value (n, s))
