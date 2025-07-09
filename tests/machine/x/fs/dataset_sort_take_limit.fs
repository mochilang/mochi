open System

exception Break
exception Continue

type Anon1 = {
    name: string
    price: int
}
let products = [{ name = "Laptop"; price = 1500 }; { name = "Smartphone"; price = 900 }; { name = "Tablet"; price = 600 }; { name = "Monitor"; price = 300 }; { name = "Keyboard"; price = 100 }; { name = "Mouse"; price = 50 }; { name = "Headphones"; price = 200 }]
let expensive = [ for p in products do yield p ] |> List.sortByDescending (fun p -> p.price) |> List.skip 1 |> List.take 3
printfn "%s" "--- Top products (excluding most expensive) ---"
try
    for item in expensive do
        try
            printfn "%s" (String.concat " " [string item.name; string "costs $"; string item.price])
        with Continue -> ()
with Break -> ()
