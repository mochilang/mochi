open System

exception Break
exception Continue

let mutable i = 0
try
    while i < 3 do
        try
            printfn "%A" (i)
            i <- i + 1
        with Continue -> ()
with Break -> ()
