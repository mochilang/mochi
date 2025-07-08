open System

exception Break
exception Continue

let customers = [dict [(id, 1); (name, "Alice")]; dict [(id, 2); (name, "Bob")]; dict [(id, 3); (name, "Charlie")]]
let orders = [dict [(id, 100); (customerId, 1)]; dict [(id, 101); (customerId, 1)]; dict [(id, 102); (customerId, 2)]]
let stats = [ for c in customers doyield dict [(name, g.key); (count, List.length [ for r in g do if r.o then yield r ])] ]
printfn "%s" "--- Group Left Join ---"
try
    for s in stats do
        try
            printfn "%s" (String.concat " " [string s.name; string "orders:"; string s.count])
        with Continue -> ()
with Break -> ()
