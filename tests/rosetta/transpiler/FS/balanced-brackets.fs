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
let mutable seed: int = 1
let rec prng (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max = max
    try
        seed <- ((((seed * 1103515245) + 12345) % 2147483648L + 2147483648L) % 2147483648L)
        __ret <- ((seed % max + max) % max)
        raise Return
        __ret
    with
        | Return -> __ret
and gen (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable arr: string array = [||]
        let mutable i: int = 0
        while i < n do
            arr <- Array.append arr [|"["|]
            arr <- Array.append arr [|"]"|]
            i <- i + 1
        let mutable j: int = (int (Array.length arr)) - 1
        while j > 0 do
            let k: int = prng (j + 1)
            let tmp: string = arr.[j]
            arr.[j] <- arr.[k]
            arr.[k] <- tmp
            j <- j - 1
        let mutable out: string = ""
        for ch in arr do
            out <- out + (unbox<string> ch)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and testBalanced (s: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    try
        let mutable ``open``: int = 0
        let mutable i: int = 0
        while i < (String.length s) do
            let c: string = s.Substring(i, (i + 1) - i)
            if c = "[" then
                ``open`` <- ``open`` + 1
            else
                if c = "]" then
                    if ``open`` = 0 then
                        printfn "%s" (s + ": not ok")
                        __ret <- ()
                        raise Return
                    ``open`` <- ``open`` - 1
                else
                    printfn "%s" (s + ": not ok")
                    __ret <- ()
                    raise Return
            i <- i + 1
        if ``open`` = 0 then
            printfn "%s" (s + ": ok")
        else
            printfn "%s" (s + ": not ok")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable i: int = 0
        while i < 10 do
            testBalanced (unbox<string> (gen i))
            i <- i + 1
        testBalanced "()"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
