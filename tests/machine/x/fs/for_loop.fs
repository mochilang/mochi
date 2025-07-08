open System

exception Break
exception Continue

try
    for i in 1 .. 4 do
        try
            printfn "%A" (i)
        with Continue -> ()
with Break -> ()
