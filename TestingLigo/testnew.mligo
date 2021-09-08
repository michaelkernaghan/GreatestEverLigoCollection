// This is testnew.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)

let test =
  let initial_storage = 42 in
  let (taddr, _,_,_) = Test.originate main initial_storage 0tez in
  assert (Test.get_storage taddr = initial_storage)

let test2 =
  let initial_storage = 42 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (1)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 1)