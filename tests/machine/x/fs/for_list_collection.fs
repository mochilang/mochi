open System

exception Break
exception Continue

try
    for n in [1; 2; 3] do
        try
            printfn "%A" (n)
        with Continue -> ()
with Break -> ()
