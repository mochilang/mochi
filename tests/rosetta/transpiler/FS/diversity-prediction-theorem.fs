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

let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < n do
            r <- r * 10.0
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and formatFloat (f: float) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    let mutable prec = prec
    try
        let scale: float = pow10 prec
        let scaled: float = (f * scale) + 0.5
        let mutable n: int = int scaled
        let mutable digits: string = string n
        while (String.length digits) <= prec do
            digits <- "0" + digits
        let intPart: string = _substring digits 0 ((String.length digits) - prec)
        let fracPart: string = _substring digits ((String.length digits) - prec) (String.length digits)
        __ret <- (intPart + ".") + fracPart
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and averageSquareDiff (f: float) (preds: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable preds = preds
    try
        let mutable av: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length preds) do
            av <- av + (((preds.[i]) - f) * ((preds.[i]) - f))
            i <- i + 1
        av <- av / (float (Seq.length preds))
        __ret <- av
        raise Return
        __ret
    with
        | Return -> __ret
and diversityTheorem (truth: float) (preds: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable truth = truth
    let mutable preds = preds
    try
        let mutable av: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length preds) do
            av <- av + (preds.[i])
            i <- i + 1
        av <- av / (float (Seq.length preds))
        let avErr: float = averageSquareDiff truth preds
        let crowdErr: float = (truth - av) * (truth - av)
        let div: float = averageSquareDiff av preds
        __ret <- unbox<float array> [|avErr; crowdErr; div|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let predsArray: float array array = [|[|48.0; 47.0; 51.0|]; [|48.0; 47.0; 51.0; 42.0|]|]
        let truth: float = 49.0
        let mutable i: int = 0
        while i < (Seq.length predsArray) do
            let preds: float array = predsArray.[i]
            let res: float array = diversityTheorem truth preds
            printfn "%s" ("Average-error : " + (unbox<string> (padLeft (formatFloat (res.[0]) 3) 6)))
            printfn "%s" ("Crowd-error   : " + (unbox<string> (padLeft (formatFloat (res.[1]) 3) 6)))
            printfn "%s" ("Diversity     : " + (unbox<string> (padLeft (formatFloat (res.[2]) 3) 6)))
            printfn "%s" ""
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
