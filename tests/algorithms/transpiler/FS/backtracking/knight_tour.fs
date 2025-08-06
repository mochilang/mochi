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
let rec get_valid_pos (position: int array) (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable position = position
    let mutable n = n
    try
        let y: int = _idx position (0)
        let x: int = _idx position (1)
        let positions: int array array = [|[|y + 1; x + 2|]; [|y - 1; x + 2|]; [|y + 1; x - 2|]; [|y - 1; x - 2|]; [|y + 2; x + 1|]; [|y + 2; x - 1|]; [|y - 2; x + 1|]; [|y - 2; x - 1|]|]
        let mutable permissible: int array array = [||]
        for idx in 0 .. ((Seq.length(positions)) - 1) do
            let inner: int array = _idx positions (idx)
            let y_test: int = _idx inner (0)
            let x_test: int = _idx inner (1)
            if (((y_test >= 0) && (y_test < n)) && (x_test >= 0)) && (x_test < n) then
                permissible <- Array.append permissible [|inner|]
        __ret <- permissible
        raise Return
        __ret
    with
        | Return -> __ret
and is_complete (board: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    try
        for i in 0 .. ((Seq.length(board)) - 1) do
            let mutable row: int array = _idx board (i)
            for j in 0 .. ((Seq.length(row)) - 1) do
                if (_idx row (j)) = 0 then
                    __ret <- false
                    raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and open_knight_tour_helper (board: int array array) (pos: int array) (curr: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    let mutable pos = pos
    let mutable curr = curr
    try
        if is_complete (board) then
            __ret <- true
            raise Return
        let moves: int array array = get_valid_pos (pos) (Seq.length(board))
        for i in 0 .. ((Seq.length(moves)) - 1) do
            let position: int array = _idx moves (i)
            let y: int = _idx position (0)
            let x: int = _idx position (1)
            if (_idx (_idx board (y)) (x)) = 0 then
                board.[y].[x] <- curr + 1
                if open_knight_tour_helper (board) (position) (curr + 1) then
                    __ret <- true
                    raise Return
                board.[y].[x] <- 0
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and open_knight_tour (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable board: int array array = [||]
        for i in 0 .. (n - 1) do
            let mutable row: int array = [||]
            for j in 0 .. (n - 1) do
                row <- Array.append row [|0|]
            board <- Array.append board [|row|]
        for i in 0 .. (n - 1) do
            for j in 0 .. (n - 1) do
                board.[i].[j] <- 1
                if open_knight_tour_helper (board) (unbox<int array> [|i; j|]) (1) then
                    __ret <- board
                    raise Return
                board.[i].[j] <- 0
        printfn "%s" ("Open Knight Tour cannot be performed on a board of size " + (_str (n)))
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
let mutable board: int array array = open_knight_tour (1)
printfn "%d" (_idx (_idx board (0)) (0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
