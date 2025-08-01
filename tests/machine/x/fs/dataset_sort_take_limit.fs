// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:46:35Z
open System

type Anon1 = {
    name: string
    price: int
}
let products: Anon1 list = [{ name = "Laptop"; price = 1500 }; { name = "Smartphone"; price = 900 }; { name = "Tablet"; price = 600 }; { name = "Monitor"; price = 300 }; { name = "Keyboard"; price = 100 }; { name = "Mouse"; price = 50 }; { name = "Headphones"; price = 200 }]
let expensive: Anon1 list = [ for p in products do yield (-p.price, p) ] |> List.sortByDescending fst |> List.map snd |> List.skip 1 |> List.take 3
printfn "%s" "--- Top products (excluding most expensive) ---"
for item in expensive do
    printfn "%s" (String.concat " " [string item.name; string "costs $"; string item.price])
