// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type HeapItem = {
    mutable _value: int
    mutable _list_idx: int
    mutable _elem_idx: int
}
let INF: int = 1000000000
let rec smallest_range (nums: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable heap: HeapItem array = [||]
        let mutable current_max: int = -INF
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let first_val: int = _idx (_idx nums (i)) (0)
            heap <- Array.append heap [|{ _value = first_val; _list_idx = i; _elem_idx = 0 }|]
            if first_val > current_max then
                current_max <- first_val
            i <- i + 1
        let mutable best: int array = [|-INF; INF|]
        try
            while (Seq.length (heap)) > 0 do
                try
                    let mutable min_idx: int = 0
                    let mutable j: int = 1
                    while j < (Seq.length (heap)) do
                        let hj: HeapItem = _idx heap (j)
                        let hmin: HeapItem = _idx heap (min_idx)
                        if (hj._value) < (hmin._value) then
                            min_idx <- j
                        j <- j + 1
                    let item: HeapItem = _idx heap (min_idx)
                    let mutable new_heap: HeapItem array = [||]
                    let mutable k: int = 0
                    while k < (Seq.length (heap)) do
                        if k <> min_idx then
                            new_heap <- Array.append new_heap [|(_idx heap (k))|]
                        k <- k + 1
                    heap <- new_heap
                    let current_min: int = item._value
                    if (current_max - current_min) < ((_idx best (1)) - (_idx best (0))) then
                        best <- unbox<int array> [|current_min; current_max|]
                    if (item._elem_idx) = ((Seq.length (_idx nums (item._list_idx))) - 1) then
                        raise Break
                    let next_val: int = _idx (_idx nums (item._list_idx)) ((item._elem_idx) + 1)
                    heap <- Array.append heap [|{ _value = next_val; _list_idx = item._list_idx; _elem_idx = (item._elem_idx) + 1 }|]
                    if next_val > current_max then
                        current_max <- next_val
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- best
        raise Return
        __ret
    with
        | Return -> __ret
and list_to_string (arr: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            s <- s + (_str (_idx arr (i)))
            if i < ((Seq.length (arr)) - 1) then
                s <- s + ", "
            i <- i + 1
        __ret <- s + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result1: int array = smallest_range ([|[|4; 10; 15; 24; 26|]; [|0; 9; 12; 20|]; [|5; 18; 22; 30|]|])
        printfn "%s" (list_to_string (result1))
        let result2: int array = smallest_range ([|[|1; 2; 3|]; [|1; 2; 3|]; [|1; 2; 3|]|])
        printfn "%s" (list_to_string (result2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
