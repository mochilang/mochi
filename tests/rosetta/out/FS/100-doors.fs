// Generated by Mochi compiler v0.10.30 on 2025-07-19T01:07:07Z
module Program
open System


let mutable doors: bool array = [||]
for i in 0 .. (100 - 1) do
    doors <- Array.append doors [|false|]
for pass in 1 .. (101 - 1) do
    let mutable idx: int = pass - 1
    while idx < 100 do
        doors.[idx] <- not doors.[idx]
        idx <- idx + pass
for row in 0 .. (10 - 1) do
    let mutable line: string = ""
    for col in 0 .. (10 - 1) do
        let idx: int = row * 10 + col
        if doors.[idx] then
            line <- line + "1"
        else
            line <- line + "0"
        if col < 9 then
            line <- line + " "
    printfn "%s" line
