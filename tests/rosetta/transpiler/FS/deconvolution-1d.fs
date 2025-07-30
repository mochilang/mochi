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
let rec listToStringInts (xs: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (int (xs.[i])))
            if i < ((Seq.length xs) - 1) then
                s <- s + " "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and deconv (g: float array) (f: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable g = g
    let mutable f = f
    try
        let mutable h: float array = [||]
        let mutable n: int = 0
        let hn: int = ((Seq.length g) - (Seq.length f)) + 1
        while n < hn do
            let mutable v: float = g.[n]
            let mutable lower: int = 0
            if n >= (Seq.length f) then
                lower <- (n - (Seq.length f)) + 1
            let mutable i: int = lower
            while i < n do
                v <- v - ((h.[i]) * (f.[n - i]))
                i <- i + 1
            v <- v / (f.[0])
            h <- Array.append h [|v|]
            n <- n + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable h: float array = [|-8.0; -9.0; -3.0; -1.0; -6.0; 7.0|]
        let f: float array = [|-3.0; -6.0; -1.0; 8.0; -6.0; 3.0; -1.0; -9.0; -9.0; 3.0; -2.0; 5.0; 2.0; -2.0; -7.0; -1.0|]
        let g: float array = [|24.0; 75.0; 71.0; -34.0; 3.0; 22.0; -45.0; 23.0; 245.0; 25.0; 52.0; 25.0; -67.0; -96.0; 96.0; 31.0; 55.0; 36.0; 29.0; -43.0; -7.0|]
        printfn "%s" (listToStringInts h)
        printfn "%s" (listToStringInts (deconv g f))
        printfn "%s" (listToStringInts f)
        printfn "%s" (listToStringInts (deconv g h))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
