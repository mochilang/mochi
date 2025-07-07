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
]

let result =
    [ for o in orders do
        match customers |> List.tryFind (fun c -> c.id = o.customerId) with
        | Some c ->
            let item = items |> List.tryFind (fun i -> i.orderId = o.id)
            yield {| orderId = o.id; name = c.name; item = item |}
        | None -> () ]

printfn "--- Left Join Multi ---"
for r in result do
    printfn "%d %s %A" r.orderId r.name r.item
