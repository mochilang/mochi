// Generated 2025-07-28 10:03 +0700

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

let rec log2 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable k: float = 0.0
        let mutable v: float = x
        while v >= 2.0 do
            v <- v / 2.0
            k <- k + 1.0
        while v < 1.0 do
            v <- v * 2.0
            k <- k - 1.0
        let z: float = (v - 1.0) / (v + 1.0)
        let mutable zpow: float = z
        let mutable sum: float = z
        let mutable i: int = 3
        while i <= 9 do
            zpow <- (zpow * z) * z
            sum <- sum + (zpow / (float i))
            i <- i + 2
        let ln2: float = 0.6931471805599453
        __ret <- k + ((2.0 * sum) / ln2)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let s: string = "1223334444"
        let mutable counts: Map<string, int> = Map.ofList []
        let mutable l: float = 0.0
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if Map.containsKey ch counts then
                counts <- Map.add ch ((int (counts.[ch] |> unbox<int>)) + 1) counts
            else
                counts <- Map.add ch 1 counts
            l <- l + 1.0
            i <- i + 1
        let mutable hm: float = 0.0
        for KeyValue(ch, _) in counts do
            let c: float = float (counts.[ch] |> unbox<int>)
            hm <- hm + (float (c * (float (log2 c))))
        printfn "%s" (string ((float (log2 l)) - (hm / l)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
