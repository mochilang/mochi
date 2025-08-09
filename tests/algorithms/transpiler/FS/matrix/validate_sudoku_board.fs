// Generated 2025-08-09 10:14 +0700

exception Break
exception Continue

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let NUM_SQUARES: int = 9
let EMPTY_CELL: string = "."
let rec is_valid_sudoku_board (board: string array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable board = board
    try
        if (Seq.length (board)) <> NUM_SQUARES then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < NUM_SQUARES do
            if (Seq.length (_idx board (int i))) <> NUM_SQUARES then
                __ret <- false
                raise Return
            i <- i + 1
        let mutable rows: string array array = Array.empty<string array>
        let mutable cols: string array array = Array.empty<string array>
        let mutable boxes: string array array = Array.empty<string array>
        i <- 0
        while i < NUM_SQUARES do
            rows <- Array.append rows [|[||]|]
            cols <- Array.append cols [|[||]|]
            boxes <- Array.append boxes [|[||]|]
            i <- i + 1
        try
            for r in 0 .. (NUM_SQUARES - 1) do
                try
                    try
                        for c in 0 .. (NUM_SQUARES - 1) do
                            try
                                let value: string = _idx (_idx board (int r)) (int c)
                                if value = EMPTY_CELL then
                                    raise Continue
                                let box: int = int ((float ((int64 (int (int (_floordiv r 3)))) * (int64 3))) + (float (int (_floordiv c 3))))
                                if ((Seq.contains value (_idx rows (int r))) || (Seq.contains value (_idx cols (int c)))) || (Seq.contains value (_idx boxes (int box))) then
                                    __ret <- false
                                    raise Return
                                rows.[int r] <- Array.append (_idx rows (int r)) [|value|]
                                cols.[int c] <- Array.append (_idx cols (int c)) [|value|]
                                boxes.[int box] <- Array.append (_idx boxes (int box)) [|value|]
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let valid_board: string array array = [|[|"5"; "3"; "."; "."; "7"; "."; "."; "."; "."|]; [|"6"; "."; "."; "1"; "9"; "5"; "."; "."; "."|]; [|"."; "9"; "8"; "."; "."; "."; "."; "6"; "."|]; [|"8"; "."; "."; "."; "6"; "."; "."; "."; "3"|]; [|"4"; "."; "."; "8"; "."; "3"; "."; "."; "1"|]; [|"7"; "."; "."; "."; "2"; "."; "."; "."; "6"|]; [|"."; "6"; "."; "."; "."; "."; "2"; "8"; "."|]; [|"."; "."; "."; "4"; "1"; "9"; "."; "."; "5"|]; [|"."; "."; "."; "."; "8"; "."; "."; "7"; "9"|]|]
let invalid_board: string array array = [|[|"8"; "3"; "."; "."; "7"; "."; "."; "."; "."|]; [|"6"; "."; "."; "1"; "9"; "5"; "."; "."; "."|]; [|"."; "9"; "8"; "."; "."; "."; "."; "6"; "."|]; [|"8"; "."; "."; "."; "6"; "."; "."; "."; "3"|]; [|"4"; "."; "."; "8"; "."; "3"; "."; "."; "1"|]; [|"7"; "."; "."; "."; "2"; "."; "."; "."; "6"|]; [|"."; "6"; "."; "."; "."; "."; "2"; "8"; "."|]; [|"."; "."; "."; "4"; "1"; "9"; "."; "."; "5"|]; [|"."; "."; "."; "."; "8"; "."; "."; "7"; "9"|]|]
printfn "%b" (is_valid_sudoku_board (valid_board))
printfn "%b" (is_valid_sudoku_board (invalid_board))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
