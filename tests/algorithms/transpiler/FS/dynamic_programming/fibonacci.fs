// Generated 2025-08-07 15:46 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Fibonacci = {
    sequence: int array
}
type FibGetResult = {
    fib: Fibonacci
    values: int array
}
let rec create_fibonacci () =
    let mutable __ret : Fibonacci = Unchecked.defaultof<Fibonacci>
    try
        __ret <- { sequence = [|0; 1|] }
        raise Return
        __ret
    with
        | Return -> __ret
and fib_get (f: Fibonacci) (index: int) =
    let mutable __ret : FibGetResult = Unchecked.defaultof<FibGetResult>
    let mutable f = f
    let mutable index = index
    try
        let mutable seq: int array = f.sequence
        while (Seq.length (seq)) < index do
            let next: int = (_idx seq ((Seq.length (seq)) - 1)) + (_idx seq ((Seq.length (seq)) - 2))
            seq <- Array.append seq [|next|]
        f <- { f with sequence = seq }
        let mutable result: int array = [||]
        let mutable i: int = 0
        while i < index do
            result <- Array.append result [|_idx seq (i)|]
            i <- i + 1
        __ret <- { fib = f; values = result }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable fib: Fibonacci = create_fibonacci()
        let mutable res: FibGetResult = fib_get (fib) (10)
        fib <- res.fib
        printfn "%s" (_str (res.values))
        res <- fib_get (fib) (5)
        fib <- res.fib
        printfn "%s" (_str (res.values))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
