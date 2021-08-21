type storage = {
  owner : address;
  amount: tez
}

type returnType  = operation list * storage

let fail (text : string) : returnType =
    ([] : operation list), (failwith(text) : storage)

let deposit (store: storage) : storage =
  {store with amount = Tezos.amount}

let main (action, store : unit * storage) : returnType =
 if Tezos.sender <> store.owner then
    fail ("Sender is not manager" )
 else
    ([] : operation list), deposit(store)