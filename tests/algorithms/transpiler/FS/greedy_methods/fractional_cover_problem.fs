// Generated 2025-08-14 16:28 +0700

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
type Item = {
    mutable _weight: int
    mutable _value: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec ratio (item: Item) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable item = item
    try
        __ret <- (float (item._value)) / (float (item._weight))
        raise Return
        __ret
    with
        | Return -> __ret
and fractional_cover (items: Item array) (capacity: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable items = items
    let mutable capacity = capacity
    try
        if capacity < 0 then
            ignore (failwith ("Capacity cannot be negative"))
        let mutable total: float = 0.0
        let mutable remaining: int = capacity
        let sorted = [| for it in Array.sortBy (        fun it -> (-(ratio (unbox<Item> it)))) (items) do yield it |]
        let mutable idx: int = 0
        while (idx < (int (Array.length (sorted)))) && (remaining > 0) do
            let item: obj = box (sorted.[idx])
            let take = if (int (((item :?> Item)._weight))) < remaining then (((item :?> Item)._weight)) else remaining
            total <- total + ((float take) * (ratio (unbox<Item> item)))
            remaining <- int (remaining - (int take))
            idx <- idx + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let items1: Item array = unbox<Item array> [|{ _weight = 10; _value = 60 }; { _weight = 20; _value = 100 }; { _weight = 30; _value = 120 }|]
ignore (printfn "%s" (_str (fractional_cover (items1) (50))))
let items2: Item array = unbox<Item array> [|{ _weight = 20; _value = 100 }; { _weight = 30; _value = 120 }; { _weight = 10; _value = 60 }|]
ignore (printfn "%s" (_str (fractional_cover (items2) (25))))
let items3: Item array = Array.empty<Item>
ignore (printfn "%s" (_str (fractional_cover (items3) (50))))
let items4: Item array = unbox<Item array> [|{ _weight = 10; _value = 60 }|]
ignore (printfn "%s" (_str (fractional_cover (items4) (5))))
ignore (printfn "%s" (_str (fractional_cover (items4) (1))))
ignore (printfn "%s" (_str (fractional_cover (items4) (0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
