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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and fmt1 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        let mutable y: float = (float (int ((x * 10.0) + 0.5))) / 10.0
        let mutable s: string = string y
        let dot: int = s.IndexOf(".")
        if dot < 0 then
            s <- s + ".0"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and listToString1 (xs: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (unbox<string> (fmt1 (xs.[i])))
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
        let mutable out: float array = [||]
        let mutable i: int = 0
        while i <= ((Seq.length g) - (Seq.length f)) do
            let mutable sum: float = g.[i]
            let mutable j: int = 1
            while j < (Seq.length f) do
                if j <= i then
                    sum <- sum - ((out.[i - j]) * (f.[j]))
                j <- j + 1
            out <- Array.append out [|sum / (f.[0])|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let h: float array = [|-8.0; -9.0; -3.0; -1.0; -6.0; 7.0|]
        let f: float array = [|-3.0; -6.0; -1.0; 8.0; -6.0; 3.0; -1.0; -9.0; -9.0; 3.0; -2.0; 5.0; 2.0; -2.0; -7.0; -1.0|]
        let g: float array = [|24.0; 75.0; 71.0; -34.0; 3.0; 22.0; -45.0; 23.0; 245.0; 25.0; 52.0; 25.0; -67.0; -96.0; 96.0; 31.0; 55.0; 36.0; 29.0; -43.0; -7.0|]
        printfn "%s" (listToString1 h)
        printfn "%s" (listToString1 (deconv g f))
        printfn "%s" (listToString1 f)
        printfn "%s" (listToString1 (deconv g h))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
