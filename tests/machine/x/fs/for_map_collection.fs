open System

exception Break
exception Continue

let mutable m = dict [("a", 1); ("b", 2)]
try
    for k in m do
        try
            printfn "%A" (k)
        with Continue -> ()
with Break -> ()
