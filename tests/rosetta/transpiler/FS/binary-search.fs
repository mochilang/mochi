// Generated 2025-07-26 04:38 +0700

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
let rec bsearch (arr: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable x = x
    try
        let mutable low: int = 0
        let mutable high: int = (int (Array.length arr)) - 1
        while low <= high do
            let mid: int = (low + high) / 2
            if (int (arr.[mid])) > x then
                high <- mid - 1
            else
                if (int (arr.[mid])) < x then
                    low <- mid + 1
                else
                    __ret <- mid
                    raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and bsearchRec (arr: int array) (x: int) (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable x = x
    let mutable low = low
    let mutable high = high
    try
        if high < low then
            __ret <- -1
            raise Return
        let mid: int = (low + high) / 2
        if (int (arr.[mid])) > x then
            __ret <- bsearchRec arr x low (mid - 1)
            raise Return
        else
            if (int (arr.[mid])) < x then
                __ret <- bsearchRec arr x (mid + 1) high
                raise Return
        __ret <- mid
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nums: int array = [|-31; 0; 1; 2; 2; 4; 65; 83; 99; 782|]
        let mutable x: int = 2
        let mutable idx: int = bsearch nums x
        if idx >= 0 then
            printfn "%s" ((((string x) + " is at index ") + (string idx)) + ".")
        else
            printfn "%s" ((string x) + " is not found.")
        x <- 5
        idx <- bsearchRec nums x 0 (int ((int (Array.length nums)) - 1))
        if idx >= 0 then
            printfn "%s" ((((string x) + " is at index ") + (string idx)) + ".")
        else
            printfn "%s" ((string x) + " is not found.")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
