// Generated 2025-08-07 14:57 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        let mutable i: int = 2
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec prev_prime (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable p: int = n - 1
        while p >= 2 do
            if is_prime (p) then
                __ret <- p
                raise Return
            p <- p - 1
        __ret <- 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec create_table (size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable size = size
    try
        let mutable vals: int array = [||]
        let mutable i: int = 0
        while i < size do
            vals <- Array.append vals [|-1|]
            i <- i + 1
        __ret <- vals
        raise Return
        __ret
    with
        | Return -> __ret
let rec hash1 (size: int) (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable size = size
    let mutable key = key
    try
        __ret <- ((key % size + size) % size)
        raise Return
        __ret
    with
        | Return -> __ret
let rec hash2 (prime: int) (key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable prime = prime
    let mutable key = key
    try
        __ret <- prime - (((key % prime + prime) % prime))
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert_double_hash (values: int array) (size: int) (prime: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable values = values
    let mutable size = size
    let mutable prime = prime
    let mutable value = value
    try
        let mutable vals: int array = values
        let mutable idx: int = hash1 (size) (value)
        let step: int = hash2 (prime) (value)
        let mutable count: int = 0
        while ((_idx vals (idx)) <> (-1)) && (count < size) do
            idx <- (((idx + step) % size + size) % size)
            count <- count + 1
        if (_idx vals (idx)) = (-1) then
            vals.[idx] <- value
        __ret <- vals
        raise Return
        __ret
    with
        | Return -> __ret
let rec table_keys (values: int array) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int>>
    let mutable values = values
    try
        let mutable res: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            if (_idx values (i)) <> (-1) then
                res.[i] <- _idx values (i)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_example (size: int) (data: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable size = size
    let mutable data = data
    try
        let prime: int = prev_prime (size)
        let mutable table: int array = create_table (size)
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            table <- insert_double_hash (table) (size) (prime) (_idx data (i))
            i <- i + 1
        printfn "%s" (_str (table_keys (table)))
        __ret
    with
        | Return -> __ret
run_example (3) (unbox<int array> [|10; 20; 30|])
run_example (4) (unbox<int array> [|10; 20; 30|])
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
