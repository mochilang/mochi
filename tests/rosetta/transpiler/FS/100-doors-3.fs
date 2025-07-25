// Generated 2025-07-22 22:23 +0700

let mutable result: string = ""
for i in 1 .. (101 - 1) do
    let mutable j: int = 1
    while (j * j) < i do
        j <- j + 1
    if (j * j) = i then
        result <- result + "O"
    else
        result <- result + "-"
printfn "%s" result
