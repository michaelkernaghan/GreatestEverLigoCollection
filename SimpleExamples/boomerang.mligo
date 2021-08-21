type storage = unit

let main (p,s: address * storage) =
    let op: operation = 
        match ((Tezos.get_contract_opt Tezos.sender): unit contract option) with
        | None -> (failwith "UNKNOWN_ADDRESS": operation)
        | Some c -> Tezos.transaction unit Tezos.amount c in
    [op], s