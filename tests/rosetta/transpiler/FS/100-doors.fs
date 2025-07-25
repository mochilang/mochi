// Generated 2025-07-22 22:23 +0700

let mutable doors = []
for i in 0 .. (100 - 1) do
    doors <- doors @ [false]
for pass in 1 .. (101 - 1) do
    let mutable idx = pass - 1
    while idx < 100 do
        doors <- List.mapi (        fun i x -> (if i = idx then (not (List.item idx doors)) else x)) doors
        idx <- idx + pass
for row in 0 .. (10 - 1) do
    let mutable line: string = ""
    for col in 0 .. (10 - 1) do
        let idx = (row * 10) + col
        if List.item idx doors then
            line <- line + "1"
        else
            line <- line + "0"
        if col < 9 then
            line <- line + " "
    printfn "%s" line
