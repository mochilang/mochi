// Generated 2025-07-25 22:14 +0700

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
let rec intSqrt (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if x < 2 then
            __ret <- x
            raise Return
        let mutable left: int = 1
        let mutable right: int = x / 2
        let mutable ans: int = 0
        while left <= right do
            let mid: int = left + ((right - left) / 2)
            let sq: int = mid * mid
            if sq = x then
                __ret <- mid
                raise Return
            if sq < x then
                left <- mid + 1
                ans <- mid
            else
                right <- mid - 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and sumRecip (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable s: int = 1
        let limit: int = intSqrt n
        let mutable f: int = 2
        while f <= limit do
            if (n % f) = 0 then
                s <- s + (n / f)
                let f2: int = n / f
                if f2 <> f then
                    s <- s + f
            f <- f + 1
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
        let nums: int array = [|6; 28; 120; 496; 672; 8128; 30240; 32760; 523776|]
        for n in nums do
            let s: int = sumRecip (int n)
            if (int (s % (int n))) = 0 then
                let ``val`` = s / (int n)
                let mutable perfect: string = ""
                if (int ``val``) = 1 then
                    perfect <- "perfect!"
                printfn "%s" ((((("Sum of recipr. factors of " + (string n)) + " = ") + (string ``val``)) + " exactly ") + perfect)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
