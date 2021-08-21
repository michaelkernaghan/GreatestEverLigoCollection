(* CamelLigo for update Bigmaps that have pairs as values *)

type stat = {
  score        : string;
  country : string
}

type standings = (address, stat) big_map

type storage = {
  league_table : standings
}

type return = operation list * storage

let update (p, s : stat * storage) :  return =
  let stat : stat =
    {score =  String.sub 2n 1n p; country = String.sub 8n 5n Tezos.sender} in
    let updated_map: standings =
    Big_map.update Tezos.sender (Some stat) s.stat in
  let s = {s with league_table = updated_map}
  in ([] : operation list), s


let main (action, store : unit * storage) : returnType =
  update(store)