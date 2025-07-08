open System

exception Break
exception Continue

let products = [dict [(name, "Laptop"); (price, 1500)]; dict [(name, "Smartphone"); (price, 900)]; dict [(name, "Tablet"); (price, 600)]; dict [(name, "Monitor"); (price, 300)]; dict [(name, "Keyboard"); (price, 100)]; dict [(name, "Mouse"); (price, 50)]; dict [(name, "Headphones"); (price, 200)]]
let expensive = [ for p in products doyield p ] |> List.sortByDescending (fun _ -> p.price) |> List.skip 1 |> List.take 3
printfn "%s" "--- Top products (excluding most expensive) ---"
try
    for item in expensive do
        try
            printfn "%s" (String.concat " " [string item.name; string "costs $"; string item.price])
        with Continue -> ()
with Break -> ()
