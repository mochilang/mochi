open System

exception Break
exception Continue

let customers = [dict [(id, 1); (name, "Alice")]; dict [(id, 2); (name, "Bob")]; dict [(id, 3); (name, "Charlie")]]
let orders = [dict [(id, 100); (customerId, 1); (total, 250)]; dict [(id, 101); (customerId, 2); (total, 125)]; dict [(id, 102); (customerId, 1); (total, 300)]; dict [(id, 103); (customerId, 4); (total, 80)]]
let result = [ for o in orders doyield dict [(orderId, o.id); (customerName, c.name); (total, o.total)] ]
printfn "%s" "--- Orders with customer info ---"
try
    for entry in result do
        try
            printfn "%s" (String.concat " " [string "Order"; string entry.orderId; string "by"; string entry.customerName; string "- $"; string entry.total])
        with Continue -> ()
with Break -> ()
