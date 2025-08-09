// Generated 2025-08-09 15:58 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
let rec catalan_numbers (upper_limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable upper_limit = upper_limit
    try
        if upper_limit < 0 then
            panic ("Limit for the Catalan sequence must be >= 0")
            __ret <- Array.empty<int>
            raise Return
        let mutable catalans: int array = unbox<int array> [|1|]
        let mutable n: int = 1
        while n <= upper_limit do
            let mutable next_val: int = 0
            let mutable j: int = 0
            while j < n do
                next_val <- int ((int64 next_val) + ((int64 (_idx catalans (int j))) * (int64 (_idx catalans (int ((n - j) - 1))))))
                j <- j + 1
            catalans <- Array.append catalans [|next_val|]
            n <- n + 1
        __ret <- catalans
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (catalan_numbers (5)))
printfn "%s" (_str (catalan_numbers (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
