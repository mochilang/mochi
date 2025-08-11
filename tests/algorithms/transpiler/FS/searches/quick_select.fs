// Generated 2025-08-11 16:20 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec partition (data: int array) (pivot: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable data = data
    let mutable pivot = pivot
    try
        let mutable less: int array = Array.empty<int>
        let mutable equal: int array = Array.empty<int>
        let mutable greater: int array = Array.empty<int>
        for i in 0 .. ((Seq.length (data)) - 1) do
            let v: int = _idx data (int i)
            if v < pivot then
                less <- Array.append less [|v|]
            else
                if v > pivot then
                    greater <- Array.append greater [|v|]
                else
                    equal <- Array.append equal [|v|]
        __ret <- [|less; equal; greater|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec quick_select (items: int array) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable items = items
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (items))) then
            __ret <- -1
            raise Return
        let pivot: int = _idx items (int (_floordiv (Seq.length (items)) 2))
        let parts: int array array = partition (items) (pivot)
        let smaller: int array = _idx parts (int 0)
        let mutable equal: int array = _idx parts (int 1)
        let larger: int array = _idx parts (int 2)
        let count: int = Seq.length (equal)
        let m: int = Seq.length (smaller)
        if (m <= index) && (index < (m + count)) then
            __ret <- pivot
            raise Return
        else
            if index < m then
                __ret <- quick_select (smaller) (index)
                raise Return
            else
                __ret <- quick_select (larger) (index - (m + count))
                raise Return
        __ret
    with
        | Return -> __ret
let rec median (items: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable items = items
    try
        let n: int = Seq.length (items)
        let mid: int = _floordiv n 2
        if (((n % 2 + 2) % 2)) <> 0 then
            __ret <- 1.0 * (float (quick_select (items) (mid)))
            raise Return
        else
            let low: int = quick_select (items) (mid - 1)
            let high: int = quick_select (items) (mid)
            __ret <- (1.0 * (float (low + high))) / 2.0
            raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (quick_select (unbox<int array> [|2; 4; 5; 7; 899; 54; 32|]) (5)))
printfn "%s" (_str (quick_select (unbox<int array> [|2; 4; 5; 7; 899; 54; 32|]) (1)))
printfn "%s" (_str (quick_select (unbox<int array> [|5; 4; 3; 2|]) (2)))
printfn "%s" (_str (quick_select (unbox<int array> [|3; 5; 7; 10; 2; 12|]) (3)))
printfn "%s" (_str (median (unbox<int array> [|3; 2; 2; 9; 9|])))
printfn "%s" (_str (median (unbox<int array> [|2; 2; 9; 9; 9; 3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
