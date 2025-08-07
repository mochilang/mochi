// Generated 2025-08-07 10:31 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
let rec string_to_grid (s: string) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable s = s
    try
        let mutable grid: int array array = [||]
        let mutable i: int = 0
        while i < 9 do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < 9 do
                let ch: string = _substring s ((i * 9) + j) (((i * 9) + j) + 1)
                let mutable ``val``: int = 0
                if (ch <> "0") && (ch <> ".") then
                    ``val`` <- int (ch)
                row <- Array.append row [|``val``|]
                j <- j + 1
            grid <- Array.append grid [|row|]
            i <- i + 1
        __ret <- grid
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_grid (grid: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable grid = grid
    try
        for r in 0 .. (9 - 1) do
            let mutable line: string = ""
            for c in 0 .. (9 - 1) do
                line <- line + (_str (_idx (_idx grid (r)) (c)))
                if c < 8 then
                    line <- line + " "
            printfn "%s" (line)
        __ret
    with
        | Return -> __ret
let rec is_safe (grid: int array array) (row: int) (column: int) (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable grid = grid
    let mutable row = row
    let mutable column = column
    let mutable n = n
    try
        for i in 0 .. (9 - 1) do
            if ((_idx (_idx grid (row)) (i)) = n) || ((_idx (_idx grid (i)) (column)) = n) then
                __ret <- false
                raise Return
        for i in 0 .. (3 - 1) do
            for j in 0 .. (3 - 1) do
                if (_idx (_idx grid ((row - (((row % 3 + 3) % 3))) + i)) ((column - (((column % 3 + 3) % 3))) + j)) = n then
                    __ret <- false
                    raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_empty (grid: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable grid = grid
    try
        for i in 0 .. (9 - 1) do
            for j in 0 .. (9 - 1) do
                if (_idx (_idx grid (i)) (j)) = 0 then
                    __ret <- unbox<int array> [|i; j|]
                    raise Return
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
let rec solve (grid: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable grid = grid
    try
        let loc: int array = find_empty (grid)
        if (Seq.length (loc)) = 0 then
            __ret <- true
            raise Return
        let mutable row: int = _idx loc (0)
        let column: int = _idx loc (1)
        for digit in 1 .. (10 - 1) do
            if is_safe (grid) (row) (column) (digit) then
                grid.[row].[column] <- digit
                if solve (grid) then
                    __ret <- true
                    raise Return
                grid.[row].[column] <- 0
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let puzzle: string = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
let mutable grid: int array array = string_to_grid (puzzle)
printfn "%s" ("Original grid:")
print_grid (grid)
if solve (grid) then
    printfn "%s" ("\nSolved grid:")
    print_grid (grid)
else
    printfn "%s" ("\nNo solution found")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
