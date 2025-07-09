open System

exception Break
exception Continue

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    customerId: int
    total: int
}
type Anon3 = {
    orderId: obj
    orderCustomerId: obj
    pairedCustomerName: obj
    orderTotal: obj
}
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]
let orders = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }]
let result = [ for o in orders do 
  for c in customers do yield { orderId = o.id; orderCustomerId = o.customerId; pairedCustomerName = c.name; orderTotal = o.total } ]

