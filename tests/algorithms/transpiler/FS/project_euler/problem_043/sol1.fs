// Generated 2025-08-22 15:25 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_substring_divisible (num: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if ((((_idx num (int 3)) % 2 + 2) % 2)) <> 0 then
            __ret <- false
            raise Return
        if ((((((_idx num (int 2)) + (_idx num (int 3))) + (_idx num (int 4))) % 3 + 3) % 3)) <> 0 then
            __ret <- false
            raise Return
        if ((((_idx num (int 5)) % 5 + 5) % 5)) <> 0 then
            __ret <- false
            raise Return
        let primes: int array = unbox<int array> [|7; 11; 13; 17|]
        let mutable i: int = 0
        while i < (Seq.length (primes)) do
            let p: int = _idx primes (int i)
            let idx: int = i + 4
            let ``val``: int64 = (((int64 (_idx num (int idx))) * (int64 100)) + ((int64 (_idx num (int (idx + 1)))) * (int64 10))) + (int64 (_idx num (int (idx + 2))))
            if (((``val`` % (int64 p) + (int64 p)) % (int64 p))) <> (int64 0) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and remove_at (xs: int array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i <> idx then
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and digits_to_number (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            value <- int (((int64 value) * (int64 10)) + (int64 (_idx xs (int i))))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and search (prefix: int array) (remaining: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable prefix = prefix
    let mutable remaining = remaining
    try
        if (Seq.length (remaining)) = 0 then
            if is_substring_divisible (prefix) then
                __ret <- digits_to_number (prefix)
                raise Return
            __ret <- 0
            raise Return
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (remaining)) do
            let d: int = _idx remaining (int i)
            let next_prefix: int array = Array.append prefix [|d|]
            let next_remaining: int array = remove_at (remaining) (i)
            total <- total + (search (next_prefix) (next_remaining))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable digits: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            digits <- Array.append digits [|i|]
            i <- i + 1
        __ret <- search (Array.empty<int>) (digits)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (String.concat " " ([|sprintf "%s" ("solution() ="); sprintf "%d" (solution (10))|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
