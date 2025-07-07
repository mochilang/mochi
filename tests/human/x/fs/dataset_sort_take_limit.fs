open System

type Product = { name: string; price: int }

let products = [
    { name = "Laptop"; price = 1500 }
    { name = "Smartphone"; price = 900 }
    { name = "Tablet"; price = 600 }
    { name = "Monitor"; price = 300 }
    { name = "Keyboard"; price = 100 }
    { name = "Mouse"; price = 50 }
    { name = "Headphones"; price = 200 }
]

let expensive =
    products
    |> List.sortByDescending (fun p -> p.price)
    |> List.skip 1
    |> List.take 3

printfn "--- Top products (excluding most expensive) ---"
for item in expensive do
    printfn "%s costs $%d" item.name item.price

