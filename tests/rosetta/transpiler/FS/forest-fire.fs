// Generated 2025-08-01 15:22 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rows: int = 20
let cols: int = 30
let p: float = 0.01
let f: float = 0.001
let rec repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + ch
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and chance (prob: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable prob = prob
    try
        let threshold: int = int (prob * 1000.0)
        __ret <- (int ((((_now()) % 1000 + 1000) % 1000))) < threshold
        raise Return
        __ret
    with
        | Return -> __ret
and newBoard () =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    try
        let mutable b: string array array = [||]
        let mutable r: int = 0
        while r < rows do
            let mutable row: string array = [||]
            let mutable c: int = 0
            while c < cols do
                if (int ((((_now()) % 2 + 2) % 2))) = 0 then
                    row <- Array.append row [|"T"|]
                else
                    row <- Array.append row [|" "|]
                c <- c + 1
            b <- Array.append b [|row|]
            r <- r + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and step (src: string array array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable src = src
    try
        let mutable dst: string array array = [||]
        let mutable r: int = 0
        while r < rows do
            let mutable row: string array = [||]
            let mutable c: int = 0
            while c < cols do
                let mutable cell: string = (src.[r]).[c]
                let mutable next: string = cell
                if cell = "#" then
                    next <- " "
                else
                    if cell = "T" then
                        let mutable burning: bool = false
                        let mutable dr: int = -1
                        while dr <= 1 do
                            let mutable dc: int = -1
                            while dc <= 1 do
                                if (dr <> 0) || (dc <> 0) then
                                    let rr: int = r + dr
                                    let cc: int = c + dc
                                    if (((rr >= 0) && (rr < rows)) && (cc >= 0)) && (cc < cols) then
                                        if ((src.[rr]).[cc]) = "#" then
                                            burning <- true
                                dc <- dc + 1
                            dr <- dr + 1
                        if burning || (unbox<bool> (chance f)) then
                            next <- "#"
                    else
                        if chance p then
                            next <- "T"
                row <- Array.append row [|next|]
                c <- c + 1
            dst <- Array.append dst [|row|]
            r <- r + 1
        __ret <- dst
        raise Return
        __ret
    with
        | Return -> __ret
and printBoard (b: string array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        printfn "%s" ((unbox<string> (repeat "__" cols)) + "\n\n")
        let mutable r: int = 0
        while r < rows do
            let mutable line: string = ""
            let mutable c: int = 0
            while c < cols do
                let cell: string = (b.[r]).[c]
                if cell = " " then
                    line <- line + "  "
                else
                    line <- (line + " ") + cell
                c <- c + 1
            printfn "%s" (line + "\n")
            r <- r + 1
        __ret
    with
        | Return -> __ret
let mutable board: string array array = newBoard()
printBoard board
board <- step board
printBoard board
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
