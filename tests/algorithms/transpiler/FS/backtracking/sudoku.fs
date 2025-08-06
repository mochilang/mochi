// Generated 2025-08-06 20:48 +0700

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
and find_empty_location (grid: int array array) =
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
and sudoku (grid: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable grid = grid
    try
        let loc: int array = find_empty_location (grid)
        if (Seq.length(loc)) = 0 then
            __ret <- true
            raise Return
        let row: int = _idx loc (0)
        let column: int = _idx loc (1)
        for digit in 1 .. (10 - 1) do
            if is_safe (grid) (row) (column) (digit) then
                grid.[row].[column] <- digit
                if sudoku (grid) then
                    __ret <- true
                    raise Return
                grid.[row].[column] <- 0
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and print_solution (grid: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable grid = grid
    try
        for r in 0 .. ((Seq.length(grid)) - 1) do
            let mutable line: string = ""
            for c in 0 .. ((Seq.length(_idx grid (r))) - 1) do
                line <- line + (_str (_idx (_idx grid (r)) (c)))
                if c < ((Seq.length(_idx grid (r))) - 1) then
                    line <- line + " "
            printfn "%s" (line)
        __ret
    with
        | Return -> __ret
let initial_grid: int array array = [|[|3; 0; 6; 5; 0; 8; 4; 0; 0|]; [|5; 2; 0; 0; 0; 0; 0; 0; 0|]; [|0; 8; 7; 0; 0; 0; 0; 3; 1|]; [|0; 0; 3; 0; 1; 0; 0; 8; 0|]; [|9; 0; 0; 8; 6; 3; 0; 0; 5|]; [|0; 5; 0; 0; 9; 0; 6; 0; 0|]; [|1; 3; 0; 0; 0; 0; 2; 5; 0|]; [|0; 0; 0; 0; 0; 0; 0; 7; 4|]; [|0; 0; 5; 2; 0; 6; 3; 0; 0|]|]
let no_solution: int array array = [|[|5; 0; 6; 5; 0; 8; 4; 0; 3|]; [|5; 2; 0; 0; 0; 0; 0; 0; 2|]; [|1; 8; 7; 0; 0; 0; 0; 3; 1|]; [|0; 0; 3; 0; 1; 0; 0; 8; 0|]; [|9; 0; 0; 8; 6; 3; 0; 0; 5|]; [|0; 5; 0; 0; 9; 0; 6; 0; 0|]; [|1; 3; 0; 0; 0; 0; 2; 5; 0|]; [|0; 0; 0; 0; 0; 0; 0; 7; 4|]; [|0; 0; 5; 2; 0; 6; 3; 0; 0|]|]
let examples: int array array array = [|initial_grid; no_solution|]
let mutable idx: int = 0
while idx < (Seq.length(examples)) do
    printfn "%s" ("\nExample grid:\n====================")
    print_solution (_idx examples (idx))
    printfn "%s" ("\nExample grid solution:")
    if sudoku (_idx examples (idx)) then
        print_solution (_idx examples (idx))
    else
        printfn "%s" ("Cannot find a solution.")
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
