// Generated 2025-07-22 22:23 +0700

let mutable door: int = 1
let mutable incrementer: int = 0
for current in 1 .. (101 - 1) do
    let mutable line: string = ("Door " + (string current)) + " "
    if current = door then
        line <- line + "Open"
        incrementer <- incrementer + 1
        door <- (door + (2 * incrementer)) + 1
    else
        line <- line + "Closed"
    printfn "%s" line
