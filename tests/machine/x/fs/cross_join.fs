open System

exception Break
exception Continue

let customers = [{| id = 1; name = "Alice" |}; {| id = 2; name = "Bob" |}; {| id = 3; name = "Charlie" |}]
let orders = [{| id = 100; customerId = 1; total = 250 |}; {| id = 101; customerId = 2; total = 125 |}; {| id = 102; customerId = 1; total = 300 |}]
let result = [ for o in orders do
  for c in customers do yield {| orderId = o.id; orderCustomerId = o.customerId; pairedCustomerName = c.name; orderTotal = o.total |} ]
printfn "%s" "--- Cross Join: All order-customer pairs ---"
try
    for entry in result do
        try
            printfn "%s" (String.concat " " [string "Order"; string entry.orderId; string "(customerId:"; string entry.orderCustomerId; string ", total: $"; string entry.orderTotal; string ") paired with"; string entry.pairedCustomerName])
        with Continue -> ()
with Break -> ()
