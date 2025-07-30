// Generated 2025-07-30 21:05 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec log10floor (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable p: int = 0
        let mutable v: int = n
        while v >= 10 do
            v <- int (v / 10)
            p <- p + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if (i > 0) && ((((((String.length s) - i) % 3 + 3) % 3)) = 0) then
                res <- res + ","
            res <- res + (_substring s i (i + 1))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and showDistribution (sizes: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable sizes = sizes
    try
        let mutable bins: int array = [||]
        let mutable i: int = 0
        while i < 12 do
            bins <- Array.append bins [|0|]
            i <- i + 1
        let mutable total: int = 0
        for sz in sizes do
            total <- total + sz
            let mutable idx: int = 0
            if sz > 0 then
                idx <- (int (log10floor sz)) + 1
            bins.[idx] <- (bins.[idx]) + 1
        printfn "%s" "File size distribution:\n"
        i <- 0
        while i < (Seq.length bins) do
            let mutable prefix: string = "  "
            if i > 0 then
                prefix <- "+ "
            printfn "%s" ((((prefix + "Files less than 10 ^ ") + (string i)) + " bytes : ") + (string (bins.[i])))
            i <- i + 1
        printfn "%s" "                                  -----"
        printfn "%s" ("= Total number of files         : " + (string (Seq.length sizes)))
        printfn "%s" (("  Total size of files           : " + (unbox<string> (commatize total))) + " bytes")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let sizes: int array = [|0; 1; 9; 10; 99; 100; 1234; 50000; 730000; 8200000|]
        showDistribution sizes
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
