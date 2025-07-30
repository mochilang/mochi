// Generated 2025-07-30 21:41 +0700

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
let rec damm (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let tbl: int array array = [|[|0; 3; 1; 7; 5; 9; 8; 6; 4; 2|]; [|7; 0; 9; 2; 1; 5; 4; 8; 6; 3|]; [|4; 2; 0; 6; 8; 7; 1; 3; 5; 9|]; [|1; 7; 5; 0; 9; 8; 3; 4; 2; 6|]; [|6; 1; 2; 3; 0; 4; 5; 9; 7; 8|]; [|3; 6; 7; 4; 2; 0; 9; 5; 8; 1|]; [|5; 8; 6; 9; 7; 2; 0; 1; 3; 4|]; [|8; 9; 4; 5; 3; 6; 2; 0; 1; 7|]; [|9; 4; 3; 8; 6; 1; 7; 2; 0; 5|]; [|2; 5; 8; 1; 4; 3; 6; 7; 9; 0|]|]
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        let mutable interim: int = 0
        let mutable i: int = 0
        while i < (String.length s) do
            let digit: int = int (digits.[(s.Substring(i, (i + 1) - i))] |> unbox<int>)
            let row: int array = tbl.[interim]
            interim <- row.[digit]
            i <- i + 1
        __ret <- interim = 0
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for s in [|"5724"; "5727"; "112946"; "112949"|] do
            printfn "%s" (((unbox<string> (padLeft (unbox<string> s) 6)) + "  ") + (string (damm (unbox<string> s))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
