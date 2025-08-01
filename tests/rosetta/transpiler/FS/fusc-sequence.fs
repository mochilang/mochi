// Generated 2025-08-01 18:08 +0000

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

let rec fuscVal (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable a: int = 1
        let mutable b: int = 0
        let mutable x: int = n
        while x > 0 do
            if (((x % 2 + 2) % 2)) = 0 then
                x <- x / 2
                a <- a + b
            else
                x <- (x - 1) / 2
                b <- a + b
        if n = 0 then
            __ret <- 0
            raise Return
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and firstFusc (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < n do
            arr <- Array.append arr [|unbox<int> (fuscVal i)|]
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable neg: bool = false
        if n < 0 then
            neg <- true
            s <- _substring s 1 (String.length s)
        let mutable i: int = (String.length s) - 3
        while i >= 1 do
            s <- ((_substring s 0 i) + ",") + (_substring s i (String.length s))
            i <- i - 3
        if neg then
            __ret <- "-" + s
            raise Return
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable out: string = s
        while (String.length out) < w do
            out <- " " + out
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
        printfn "%s" "The first 61 fusc numbers are:"
        printfn "%s" (string (firstFusc 61))
        printfn "%s" "\nThe fusc numbers whose length > any previous fusc number length are:"
        let idxs: int array = [|0; 37; 1173; 35499; 699051; 19573419|]
        let mutable i: int = 0
        while i < (Seq.length idxs) do
            let idx: int = idxs.[i]
            let ``val``: int = fuscVal idx
            let numStr: string = padLeft (commatize ``val``) 7
            let idxStr: string = padLeft (commatize idx) 10
            printfn "%s" (((numStr + " (index ") + idxStr) + ")")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
