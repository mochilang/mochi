// Generated 2025-07-26 04:38 +0700

exception Return

let mutable grid: string array array = [|[|"."; "."; "."; "."; "."|]; [|"."; "#"; "#"; "#"; "."|]; [|"."; "#"; "."; "#"; "."|]; [|"."; "#"; "#"; "#"; "."|]; [|"."; "."; "."; "."; "."|]|]
let rec flood (x: int) (y: int) (repl: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x = x
    let mutable y = y
    let mutable repl = repl
    try
        let target: string = (grid.[y]).[x]
        if target = repl then
            __ret <- ()
            raise Return
        let rec ff (px: int) (py: int) =
            let mutable __ret = ()
            let mutable px = px
            let mutable py = py
            try
                if (((px < 0) || (py < 0)) || (py >= (int (Array.length grid)))) || (px >= (Seq.length (grid.[0]))) then
                    __ret <- ()
                    raise Return
                if (unbox<string> ((grid.[py]).[px])) <> target then
                    __ret <- ()
                    raise Return
                (grid.[py]).[px] <- repl
                ff (px - 1) py
                ff (px + 1) py
                ff px (py - 1)
                ff px (py + 1)
                __ret
            with
                | Return -> __ret
        ff x y
        __ret
    with
        | Return -> __ret
flood 2 2 "o"
for row in grid do
    let mutable line: string = ""
    for ch in row do
        line <- line + (unbox<string> ch)
    printfn "%s" line
