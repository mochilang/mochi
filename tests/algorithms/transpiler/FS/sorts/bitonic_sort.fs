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
let rec set_at_int (xs: int array) (idx: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and comp_and_swap (arr: int array) (i: int) (j: int) (dir: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable i = i
    let mutable j = j
    let mutable dir = dir
    try
        let mutable res: int array = arr
        let xi: int = _idx arr (int i)
        let xj: int = _idx arr (int j)
        if ((dir = 1) && (xi > xj)) || ((dir = 0) && (xi < xj)) then
            res <- set_at_int (res) (i) (xj)
            res <- set_at_int (res) (j) (xi)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bitonic_merge (arr: int array) (low: int) (length: int) (dir: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable low = low
    let mutable length = length
    let mutable dir = dir
    try
        let mutable res: int array = arr
        if length > 1 then
            let mid: int = _floordiv length 2
            let mutable k: int = low
            while k < (low + mid) do
                res <- comp_and_swap (res) (k) (k + mid) (dir)
                k <- k + 1
            res <- bitonic_merge (res) (low) (mid) (dir)
            res <- bitonic_merge (res) (low + mid) (mid) (dir)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bitonic_sort (arr: int array) (low: int) (length: int) (dir: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable low = low
    let mutable length = length
    let mutable dir = dir
    try
        let mutable res: int array = arr
        if length > 1 then
            let mid: int = _floordiv length 2
            res <- bitonic_sort (res) (low) (mid) (1)
            res <- bitonic_sort (res) (low + mid) (mid) (0)
            res <- bitonic_merge (res) (low) (length) (dir)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: int array = unbox<int array> [|12; 34; 92; -23; 0; -121; -167; 145|]
        let asc: int array = bitonic_sort (data) (0) (Seq.length (data)) (1)
        printfn "%s" (_str (asc))
        let desc: int array = bitonic_merge (asc) (0) (Seq.length (asc)) (0)
        printfn "%s" (_str (desc))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
