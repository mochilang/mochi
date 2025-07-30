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

let mutable given: string array = [|"ABCD"; "CABD"; "ACDB"; "DACB"; "BCDA"; "ACBD"; "ADCB"; "CDAB"; "DABC"; "BCAD"; "CADB"; "CDBA"; "CBAD"; "ABDC"; "ADBC"; "BDCA"; "DCBA"; "BACD"; "BADC"; "BDAC"; "CBDA"; "DBCA"; "DCAB"|]
let rec idx (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        if ch = "A" then
            __ret <- 0
            raise Return
        if ch = "B" then
            __ret <- 1
            raise Return
        if ch = "C" then
            __ret <- 2
            raise Return
        __ret <- 3
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (given.[0])) do
            let mutable counts: int array = [|0; 0; 0; 0|]
            for p in given do
                let ch: string = _substring p i (i + 1)
                let j: int = idx ch
                counts.[j] <- (counts.[j]) + 1
            let mutable j: int = 0
            while j < 4 do
                if ((((counts.[j]) % 2 + 2) % 2)) = 1 then
                    if j = 0 then
                        res <- res + "A"
                    else
                        if j = 1 then
                            res <- res + "B"
                        else
                            if j = 2 then
                                res <- res + "C"
                            else
                                res <- res + "D"
                j <- j + 1
            i <- i + 1
        printfn "%s" res
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
