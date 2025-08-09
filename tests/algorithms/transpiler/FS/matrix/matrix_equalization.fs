// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec unique (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        try
            while i < (Seq.length (nums)) do
                try
                    let v: int = _idx nums (int i)
                    let mutable found: bool = false
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (res)) do
                            try
                                if (_idx res (int j)) = v then
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        res <- Array.append res [|v|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec array_equalization (vector: int array) (step_size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable vector = vector
    let mutable step_size = step_size
    try
        if step_size <= 0 then
            failwith ("Step size must be positive and non-zero.")
        let elems: int array = unique (vector)
        let mutable min_updates: int = Seq.length (vector)
        let mutable i: int = 0
        while i < (Seq.length (elems)) do
            let target: int = _idx elems (int i)
            let mutable idx: int = 0
            let mutable updates: int = 0
            while idx < (Seq.length (vector)) do
                if (_idx vector (int idx)) <> target then
                    updates <- updates + 1
                    idx <- idx + step_size
                else
                    idx <- idx + 1
            if updates < min_updates then
                min_updates <- updates
            i <- i + 1
        __ret <- min_updates
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (array_equalization (unbox<int array> [|1; 1; 6; 2; 4; 6; 5; 1; 7; 2; 2; 1; 7; 2; 2|]) (4)))
printfn "%s" (_str (array_equalization (unbox<int array> [|22; 81; 88; 71; 22; 81; 632; 81; 81; 22; 92|]) (2)))
printfn "%s" (_str (array_equalization (unbox<int array> [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]) (5)))
printfn "%s" (_str (array_equalization (unbox<int array> [|22; 22; 22; 33; 33; 33|]) (2)))
printfn "%s" (_str (array_equalization (unbox<int array> [|1; 2; 3|]) (2147483647)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
