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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and is_pentagonal (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let root: float = sqrt (1.0 + (24.0 * (1.0 * (float n))))
        let ``val``: float = (1.0 + root) / 6.0
        let val_int: int = int (``val``)
        __ret <- ``val`` = (1.0 * (float val_int))
        raise Return
        __ret
    with
        | Return -> __ret
and pentagonal (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable k = k
    try
        __ret <- int (_floordiv64 (int64 ((int64 k) * (((int64 3) * (int64 k)) - (int64 1)))) (int64 (int64 2)))
        raise Return
        __ret
    with
        | Return -> __ret
and solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable pentagonal_nums: int array = Array.empty<int>
        let mutable i: int = 1
        while i < limit do
            pentagonal_nums <- Array.append pentagonal_nums [|(pentagonal (i))|]
            i <- i + 1
        let mutable a_idx: int = 0
        while a_idx < (Seq.length (pentagonal_nums)) do
            let pentagonal_i: int = _idx pentagonal_nums (int a_idx)
            let mutable b_idx: int = a_idx
            while b_idx < (Seq.length (pentagonal_nums)) do
                let pentagonal_j: int = _idx pentagonal_nums (int b_idx)
                let s: int = pentagonal_i + pentagonal_j
                let d: int = pentagonal_j - pentagonal_i
                if (is_pentagonal (s)) && (is_pentagonal (d)) then
                    __ret <- d
                    raise Return
                b_idx <- b_idx + 1
            a_idx <- a_idx + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let result: int = solution (5000)
ignore (printfn "%s" ("solution() = " + (_str (result))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
