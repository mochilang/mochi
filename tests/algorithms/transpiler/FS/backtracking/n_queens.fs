// Generated 2025-08-06 16:21 +0700

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
let rec create_board (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable board: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0|]
                j <- j + 1
            board <- Array.append board [|row|]
            i <- i + 1
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
and is_safe (board: int array array) (row: int) (column: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    let mutable row = row
    let mutable column = column
    try
        let n: int = Seq.length(board)
        let mutable i: int = 0
        while i < row do
            if (_idx (_idx board (i)) (column)) = 1 then
                __ret <- false
                raise Return
            i <- i + 1
        i <- row - 1
        let mutable j: int = column - 1
        while (i >= 0) && (j >= 0) do
            if (_idx (_idx board (i)) (j)) = 1 then
                __ret <- false
                raise Return
            i <- i - 1
            j <- j - 1
        i <- row - 1
        j <- column + 1
        while (i >= 0) && (j < n) do
            if (_idx (_idx board (i)) (j)) = 1 then
                __ret <- false
                raise Return
            i <- i - 1
            j <- j + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and row_string (row: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable s: string = ""
        let mutable j: int = 0
        while j < (Seq.length(row)) do
            if (_idx row (j)) = 1 then
                s <- s + "Q "
            else
                s <- s + ". "
            j <- j + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and printboard (board: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable board = board
    try
        let mutable i: int = 0
        while i < (Seq.length(board)) do
            printfn "%s" (row_string (_idx board (i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and solve (board: int array array) (row: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable board = board
    let mutable row = row
    try
        if row >= (Seq.length(board)) then
            printboard (board)
            printfn "%s" ("")
            __ret <- 1
            raise Return
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (Seq.length(board)) do
            if is_safe (board) (row) (i) then
                board.[row].[i] <- 1
                count <- count + (solve (board) (row + 1))
                board.[row].[i] <- 0
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and n_queens (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable board: int array array = create_board (n)
        let total: int = solve (board) (0)
        printfn "%s" ("The total number of solutions are: " + (_str (total)))
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
n_queens (4)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
