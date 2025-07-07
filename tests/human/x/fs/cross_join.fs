open System

type Customer = { id: int; name: string }
type Order = { id: int; customerId: int; total: int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
    { id = 3; name = "Charlie" }
]

let orders = [
    { id = 100; customerId = 1; total = 250 }
    { id = 101; customerId = 2; total = 125 }
    { id = 102; customerId = 1; total = 300 }
]

let result =
    [ for o in orders do
        for c in customers do
            yield {| orderId = o.id
                     orderCustomerId = o.customerId
                     pairedCustomerName = c.name
                     orderTotal = o.total |} ]

printfn "--- Cross Join: All order-customer pairs ---"
for entry in result do
    printfn "Order %d (customerId: %d, total: $%d) paired with %s" \
        entry.orderId entry.orderCustomerId entry.orderTotal entry.pairedCustomerName

