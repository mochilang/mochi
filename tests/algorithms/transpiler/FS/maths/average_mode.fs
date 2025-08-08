// Generated 2025-08-08 17:07 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec contains_int (xs: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_string (xs: string array) (x: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_int (xs: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_string (xs: string array) (x: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_int (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = i + 1
            while j < (Seq.length (arr)) do
                if (_idx arr (j)) < (_idx arr (i)) then
                    let tmp: int = _idx arr (i)
                    arr.[i] <- _idx arr (j)
                    arr.[j] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_string (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr: string array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = i + 1
            while j < (Seq.length (arr)) do
                if (_idx arr (j)) < (_idx arr (i)) then
                    let tmp: string = _idx arr (i)
                    arr.[i] <- _idx arr (j)
                    arr.[j] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec mode_int (lst: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable lst = lst
    try
        if (Seq.length (lst)) = 0 then
            __ret <- Array.empty<int>
            raise Return
        let mutable counts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            counts <- Array.append counts [|(count_int (lst) (_idx lst (i)))|]
            i <- i + 1
        let mutable max_count: int = 0
        i <- 0
        while i < (Seq.length (counts)) do
            if (_idx counts (i)) > max_count then
                max_count <- _idx counts (i)
            i <- i + 1
        let mutable modes: int array = Array.empty<int>
        i <- 0
        while i < (Seq.length (lst)) do
            if (_idx counts (i)) = max_count then
                let v: int = _idx lst (i)
                if not (contains_int (modes) (v)) then
                    modes <- Array.append modes [|v|]
            i <- i + 1
        __ret <- sort_int (modes)
        raise Return
        __ret
    with
        | Return -> __ret
let rec mode_string (lst: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lst = lst
    try
        if (Seq.length (lst)) = 0 then
            __ret <- Array.empty<string>
            raise Return
        let mutable counts: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            counts <- Array.append counts [|(count_string (lst) (_idx lst (i)))|]
            i <- i + 1
        let mutable max_count: int = 0
        i <- 0
        while i < (Seq.length (counts)) do
            if (_idx counts (i)) > max_count then
                max_count <- _idx counts (i)
            i <- i + 1
        let mutable modes: string array = Array.empty<string>
        i <- 0
        while i < (Seq.length (lst)) do
            if (_idx counts (i)) = max_count then
                let v: string = _idx lst (i)
                if not (contains_string (modes) (v)) then
                    modes <- Array.append modes [|v|]
            i <- i + 1
        __ret <- sort_string (modes)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (mode_int (unbox<int array> [|2; 3; 4; 5; 3; 4; 2; 5; 2; 2; 4; 2; 2; 2|])))
printfn "%s" (_repr (mode_int (unbox<int array> [|3; 4; 5; 3; 4; 2; 5; 2; 2; 4; 4; 2; 2; 2|])))
printfn "%s" (_repr (mode_int (unbox<int array> [|3; 4; 5; 3; 4; 2; 5; 2; 2; 4; 4; 4; 2; 2; 4; 2|])))
printfn "%s" (_repr (mode_string (unbox<string array> [|"x"; "y"; "y"; "z"|])))
printfn "%s" (_repr (mode_string (unbox<string array> [|"x"; "x"; "y"; "y"; "z"|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
