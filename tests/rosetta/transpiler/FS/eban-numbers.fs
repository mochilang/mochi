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

let vals: int array = [|0; 2; 4; 6; 30; 32; 34; 36; 40; 42; 44; 46; 50; 52; 54; 56; 60; 62; 64; 66|]
let billions: int array = [|0; 2; 4; 6|]
let rec ebanNumbers (start: int) (stop: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable start = start
    let mutable stop = stop
    try
        let mutable nums: int array = [||]
        for b in billions do
            for m in vals do
                for t in vals do
                    for r in vals do
                        let n: int = (((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r
                        if (n >= start) && (n <= stop) then
                            nums <- Array.append nums [|n|]
        __ret <- nums
        raise Return
        __ret
    with
        | Return -> __ret
and countEban (start: int) (stop: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable start = start
    let mutable stop = stop
    try
        let mutable count: int = 0
        for b in billions do
            for m in vals do
                for t in vals do
                    for r in vals do
                        let n: int = (((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r
                        if (n >= start) && (n <= stop) then
                            count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ranges = [|[|box 2; box 1000; box true|]; [|box 1000; box 4000; box true|]; [|box 2; box 10000; box false|]; [|box 2; box 100000; box false|]; [|box 2; box 1000000; box false|]; [|box 2; box 10000000; box false|]; [|box 2; box 100000000; box false|]; [|box 2; box 1000000000; box false|]|]
        for rg in ranges do
            let start: int = int (rg.[0])
            let stop: int = int (rg.[1])
            let show: bool = unbox<bool> (rg.[2])
            if start = 2 then
                printfn "%s" (("eban numbers up to and including " + (string stop)) + ":")
            else
                printfn "%s" (((("eban numbers between " + (string start)) + " and ") + (string stop)) + " (inclusive):")
            if show then
                let nums: int array = ebanNumbers start stop
                let mutable line: string = ""
                let mutable i: int = 0
                while i < (Seq.length nums) do
                    line <- (line + (string (nums.[i]))) + " "
                    i <- i + 1
                if (String.length line) > 0 then
                    printfn "%s" (_substring line 0 ((String.length line) - 1))
            let c: int = countEban start stop
            printfn "%s" (("count = " + (string c)) + "\n")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
