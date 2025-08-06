// Generated 2025-08-06 22:14 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec count_alive_neighbours (board: bool array array) (row: int) (col: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable board = board
    let mutable row = row
    let mutable col = col
    try
        let size: int = Seq.length (board)
        let mutable alive: int = 0
        let mutable dr: int = -1
        while dr < 2 do
            let mutable dc: int = -1
            while dc < 2 do
                let nr: int = row + dr
                let nc: int = col + dc
                if ((((not ((dr = 0) && (dc = 0))) && (nr >= 0)) && (nr < size)) && (nc >= 0)) && (nc < size) then
                    if _idx (_idx board (nr)) (nc) then
                        alive <- alive + 1
                dc <- dc + 1
            dr <- dr + 1
        __ret <- alive
        raise Return
        __ret
    with
        | Return -> __ret
and next_state (current: bool) (alive: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable current = current
    let mutable alive = alive
    try
        let mutable state: bool = current
        if current then
            if alive < 2 then
                state <- false
            else
                if (alive = 2) || (alive = 3) then
                    state <- true
                else
                    state <- false
        else
            if alive = 3 then
                state <- true
        __ret <- state
        raise Return
        __ret
    with
        | Return -> __ret
and step (board: bool array array) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable board = board
    try
        let size: int = Seq.length (board)
        let mutable new_board: bool array array = [||]
        let mutable r: int = 0
        while r < size do
            let mutable new_row: bool array = [||]
            let mutable c: int = 0
            while c < size do
                let mutable alive: int = count_alive_neighbours (board) (r) (c)
                let cell: bool = _idx (_idx board (r)) (c)
                let updated: bool = next_state (cell) (alive)
                new_row <- Array.append new_row [|updated|]
                c <- c + 1
            new_board <- Array.append new_board [|new_row|]
            r <- r + 1
        __ret <- new_board
        raise Return
        __ret
    with
        | Return -> __ret
and show (board: bool array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable board = board
    try
        let mutable r: int = 0
        while r < (Seq.length (board)) do
            let mutable line: string = ""
            let mutable c: int = 0
            while c < (Seq.length (_idx board (r))) do
                if _idx (_idx board (r)) (c) then
                    line <- line + "#"
                else
                    line <- line + "."
                c <- c + 1
            printfn "%s" (line)
            r <- r + 1
        __ret
    with
        | Return -> __ret
let glider: bool array array = [|[|false; true; false; false; false|]; [|false; false; true; false; false|]; [|true; true; true; false; false|]; [|false; false; false; false; false|]; [|false; false; false; false; false|]|]
let mutable board: bool array array = glider
printfn "%s" ("Initial")
show (board)
let mutable i: int = 0
while i < 4 do
    board <- step (board)
    printfn "%s" ("\nStep " + (_str (i + 1)))
    show (board)
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
