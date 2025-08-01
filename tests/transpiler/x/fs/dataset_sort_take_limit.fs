// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable name: string
    mutable price: int
}
type Anon2 = {
    mutable name: string
    mutable price: int
}
let products: Anon2 list = [{ name = "Laptop"; price = 1500 }; { name = "Smartphone"; price = 900 }; { name = "Tablet"; price = 600 }; { name = "Monitor"; price = 300 }; { name = "Keyboard"; price = 100 }; { name = "Mouse"; price = 50 }; { name = "Headphones"; price = 200 }]
let expensive = [ for p in List.take 3 (List.skip 1 (List.sortBy (fun p -> (-(p.price))) products)) do yield p ]
printfn "%s" (string "--- Top products (excluding most expensive) ---")
for item in expensive do
printfn "%s" (String.concat " " [string (item.name); string "costs $"; string (item.price)])
