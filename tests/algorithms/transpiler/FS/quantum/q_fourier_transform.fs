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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
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
open System.Collections.Generic

let rec to_bits (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable res: string = ""
        let mutable num: int = n
        let mutable w: int = width
        while w > 0 do
            res <- (_str (((num % 2 + 2) % 2))) + res
            num <- _floordiv num 2
            w <- w - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec quantum_fourier_transform (number_of_qubits: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int>>
    let mutable number_of_qubits = number_of_qubits
    try
        if number_of_qubits <= 0 then
            failwith ("number of qubits must be > 0.")
        if number_of_qubits > 10 then
            failwith ("number of qubits too large to simulate(>10).")
        let shots: int = 10000
        let mutable states: int = 1
        let mutable p: int = 0
        while p < number_of_qubits do
            states <- int ((int64 states) * (int64 2))
            p <- p + 1
        let per_state: int = _floordiv shots states
        let counts: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < states do
            counts.[(to_bits (i) (number_of_qubits))] <- per_state
            i <- i + 1
        __ret <- counts
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("Total count for quantum fourier transform state is: " + (_str (quantum_fourier_transform (3))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
