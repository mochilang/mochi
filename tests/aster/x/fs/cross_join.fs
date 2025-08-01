// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    id: int
    name: string
}


type Anon2 = {
    id: int
    name: string
}


type Anon3 = {
    id: int
    customerId: int
    total: int
}


type Anon4 = {
    id: int
    customerId: int
    total: int
}


type Anon5 = {
    orderId: obj
    orderCustomerId: obj
    pairedCustomerName: obj
    orderTotal: obj
}


type Anon6 = {
    orderId: obj
    orderCustomerId: obj
    pairedCustomerName: obj
    orderTotal: obj
}


let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]

let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }]

let result: Anon6 list = [for o in orders do
for c in customers do


]

printfn "%s" (string "--- Cross Join: All order-customer pairs ---")

entry

result

printfn "%s"

(String.concat " " [string "Order"; string (entry.orderId); string "(customerId:"; string (entry.orderCustomerId); string ", total: $"; string (entry.orderTotal); string ") paired with"; string (entry.pairedCustomerName)])
