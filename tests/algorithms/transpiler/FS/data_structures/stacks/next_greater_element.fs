// Generated 2025-08-08 11:10 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let arr: float array = [|-10.0; -5.0; 0.0; 5.0; 5.1; 11.0; 13.0; 21.0; 3.0; 4.0; -21.0; -10.0; -5.0; -1.0; 0.0|]
let expected: float array = [|-5.0; 0.0; 5.0; 5.1; 11.0; 13.0; 21.0; -1.0; 4.0; -1.0; -10.0; -5.0; -1.0; 0.0; -1.0|]
let rec next_greatest_element_slow (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        try
            while i < (Seq.length (xs)) do
                try
                    let mutable next: float = -1.0
                    let mutable j: int = i + 1
                    try
                        while j < (Seq.length (xs)) do
                            try
                                if (_idx xs (i)) < (_idx xs (j)) then
                                    next <- _idx xs (j)
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    res <- Array.append res [|next|]
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
let rec next_greatest_element_fast (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        try
            while i < (Seq.length (xs)) do
                try
                    let mutable next: float = -1.0
                    let mutable j: int = i + 1
                    try
                        while j < (Seq.length (xs)) do
                            try
                                let inner: float = _idx xs (j)
                                if (_idx xs (i)) < inner then
                                    next <- inner
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    res <- Array.append res [|next|]
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
let rec set_at_float (xs: float array) (idx: int) (value: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable i: int = 0
        let mutable res: float array = [||]
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec next_greatest_element (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = [||]
        let mutable k: int = 0
        while k < (Seq.length (xs)) do
            res <- Array.append res [|(-1.0)|]
            k <- k + 1
        let mutable stack: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            while ((Seq.length (stack)) > 0) && ((_idx xs (i)) > (_idx xs (_idx stack ((Seq.length (stack)) - 1)))) do
                let idx: int = _idx stack ((Seq.length (stack)) - 1)
                stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                res <- set_at_float (res) (idx) (_idx xs (i))
            stack <- Array.append stack [|i|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (next_greatest_element_slow (arr)))
printfn "%s" (_str (next_greatest_element_fast (arr)))
printfn "%s" (_str (next_greatest_element (arr)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
