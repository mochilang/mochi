open System

type Customer = { id:int; name:string }
type Order = { id:int; customerId:int }
type Item = { orderId:int; sku:string }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
]

let orders = [
    { id = 100; customerId = 1 }
    { id = 101; customerId = 2 }
]

let items = [
    { orderId = 100; sku = "a" }
    { orderId = 101; sku = "b" }
]

let result =
    [ for o in orders do
        match customers |> List.tryFind (fun c -> c.id = o.customerId) with
        | Some c ->
            for i in items do
                if i.orderId = o.id then
                    yield {| name = c.name; sku = i.sku |}
        | None -> () ]

printfn "--- Multi Join ---"
for r in result do
    printfn "%s bought item %s" r.name r.sku
