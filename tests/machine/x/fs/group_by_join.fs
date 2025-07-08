open System

exception Break
exception Continue

let customers = [dict [(id, 1); (name, "Alice")]; dict [(id, 2); (name, "Bob")]]
let orders = [dict [(id, 100); (customerId, 1)]; dict [(id, 101); (customerId, 1)]; dict [(id, 102); (customerId, 2)]]
let stats = [ for o in orders doyield dict [(name, g.key); (count, List.length g)] ]
printfn "%s" "--- Orders per customer ---"
try
    for s in stats do
        try
            printfn "%s" (String.concat " " [string s.name; string "orders:"; string s.count])
        with Continue -> ()
with Break -> ()
