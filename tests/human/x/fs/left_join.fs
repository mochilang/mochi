open System

type Customer = { id:int; name:string }
type Order = { id:int; customerId:int; total:int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
]

let orders = [
    { id = 100; customerId = 1; total = 250 }
    { id = 101; customerId = 3; total = 80 }
]

let result =
    [ for o in orders do
        let customer = customers |> List.tryFind (fun c -> c.id = o.customerId)
        yield {| orderId = o.id; customer = customer; total = o.total |} ]

printfn "--- Left Join ---"
for entry in result do
    printfn "Order %d customer %A total %d" entry.orderId entry.customer entry.total
