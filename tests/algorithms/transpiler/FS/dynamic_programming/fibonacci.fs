// Generated 2025-08-13 07:12 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Fibonacci = {
    mutable _sequence: int array
}
type FibGetResult = {
    mutable _fib: Fibonacci
    mutable _values: int array
}
let rec create_fibonacci () =
    let mutable __ret : Fibonacci = Unchecked.defaultof<Fibonacci>
    try
        __ret <- { _sequence = unbox<int array> [|0; 1|] }
        raise Return
        __ret
    with
        | Return -> __ret
and fib_get (f: Fibonacci) (index: int) =
    let mutable __ret : FibGetResult = Unchecked.defaultof<FibGetResult>
    let mutable f = f
    let mutable index = index
    try
        let mutable seq: int array = f._sequence
        while (Seq.length (seq)) < index do
            let next: int = (_idx seq (int ((Seq.length (seq)) - 1))) + (_idx seq (int ((Seq.length (seq)) - 2)))
            seq <- Array.append seq [|next|]
        f._sequence <- seq
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        while i < index do
            result <- Array.append result [|(_idx seq (int i))|]
            i <- i + 1
        __ret <- { _fib = f; _values = result }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable _fib: Fibonacci = create_fibonacci()
        let mutable res: FibGetResult = fib_get (_fib) (10)
        _fib <- res._fib
        ignore (printfn "%s" (_str (res._values)))
        res <- fib_get (_fib) (5)
        _fib <- res._fib
        ignore (printfn "%s" (_str (res._values)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
