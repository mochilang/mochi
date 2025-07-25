// Generated 2025-07-26 05:05 +0700

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
let rec pfacSum (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        let mutable sum: int = 0
        let mutable p: int = 1
        while p <= (i / 2) do
            if (((i % p + p) % p)) = 0 then
                sum <- sum + p
            p <- p + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
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
        let mutable sums: int array = [||]
        let mutable i: int = 0
        while i < 20000 do
            sums <- Array.append sums [|0|]
            i <- i + 1
        i <- 1
        while i < 20000 do
            sums.[i] <- pfacSum i
            i <- i + 1
        printfn "%s" "The amicable pairs below 20,000 are:"
        let mutable n: int = 2
        while n < 19999 do
            let m: int = sums.[n]
            if ((m > n) && (m < 20000)) && (n = (int (sums.[m]))) then
                printfn "%s" ((("  " + (unbox<string> (pad n 5))) + " and ") + (unbox<string> (pad m 5)))
            n <- n + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
