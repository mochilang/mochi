// Generated 2025-07-26 05:05 +0700

let msg: string = "Hello World! "
let mutable shift: int = 0
let mutable inc: int = 1
let mutable clicks: int = 0
let mutable frames: int = 0
while clicks < 5 do
    let mutable line: string = ""
    let mutable i: int = 0
    while i < (String.length msg) do
        let idx: int = (((shift + i) % (String.length msg) + (String.length msg)) % (String.length msg))
        line <- line + (msg.Substring(idx, (idx + 1) - idx))
        i <- i + 1
    printfn "%s" line
    shift <- (((shift + inc) % (String.length msg) + (String.length msg)) % (String.length msg))
    frames <- frames + 1
    if (((frames % (String.length msg) + (String.length msg)) % (String.length msg))) = 0 then
        inc <- (String.length msg) - inc
        clicks <- clicks + 1
