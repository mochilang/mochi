open System

exception Break
exception Continue

let mutable m = dict [("a", 1); ("b", 2)]
try
    for KeyValue(k, _) in m do
        try
            printfn "%s" k
        with Continue -> ()
with Break -> ()
