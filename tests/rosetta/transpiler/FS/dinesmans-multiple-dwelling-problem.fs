// Generated 2025-07-31 00:10 +0700

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
let rec absInt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable b: int = 1
        while b <= 5 do
            if b <> 5 then
                let mutable c: int = 1
                while c <= 5 do
                    if (c <> 1) && (c <> b) then
                        let mutable f: int = 1
                        while f <= 5 do
                            if ((((f <> 1) && (f <> 5)) && (f <> b)) && (f <> c)) && ((int (absInt (f - c))) > 1) then
                                let mutable m: int = 1
                                while m <= 5 do
                                    if (((m <> b) && (m <> c)) && (m <> f)) && (m > c) then
                                        let mutable s: int = 1
                                        while s <= 5 do
                                            if ((((s <> b) && (s <> c)) && (s <> f)) && (s <> m)) && ((int (absInt (s - f))) > 1) then
                                                printfn "%s" (((((((((("Baker in " + (string b)) + ", Cooper in ") + (string c)) + ", Fletcher in ") + (string f)) + ", Miller in ") + (string m)) + ", Smith in ") + (string s)) + ".")
                                                __ret <- ()
                                                raise Return
                                            s <- s + 1
                                    m <- m + 1
                            f <- f + 1
                    c <- c + 1
            b <- b + 1
        printfn "%s" "No solution found."
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
