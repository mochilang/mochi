// Generated 2025-08-08 16:34 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type Machine = {
    mutable _buffer: float array
    mutable _params: float array
    mutable _time: int
}
type PullResult = {
    mutable _value: int
    mutable _machine: Machine
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let K: float array = [|0.33; 0.44; 0.55; 0.44; 0.33|]
let t: int = 3
let size: int = 5
let rec round_dec (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let mutable m10: float = 1.0
        let mutable i: int = 0
        while i < n do
            m10 <- m10 * 10.0
            i <- i + 1
        let y: float = (x * m10) + 0.5
        __ret <- (float (1.0 * (float (int (y))))) / m10
        raise Return
        __ret
    with
        | Return -> __ret
let rec reset () =
    let mutable __ret : Machine = Unchecked.defaultof<Machine>
    try
        __ret <- { _buffer = K; _params = unbox<float array> [|0.0; 0.0; 0.0; 0.0; 0.0|]; _time = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec push (m: Machine) (_seed: int) =
    let mutable __ret : Machine = Unchecked.defaultof<Machine>
    let mutable m = m
    let mutable _seed = _seed
    try
        let mutable buf: float array = m._buffer
        let mutable par: float array = m._params
        let mutable i: int = 0
        while i < (Seq.length (buf)) do
            let _value: float = _idx buf (i)
            let e: float = (1.0 * (float _seed)) / _value
            let mutable next_value: float = (_idx buf ((((i + 1) % size + size) % size))) + e
            next_value <- next_value - (float (1.0 * (float (int (next_value)))))
            let mutable r: float = (_idx par (i)) + e
            r <- r - (float (1.0 * (float (int (r)))))
            r <- r + 3.0
            buf.[i] <- round_dec ((r * next_value) * (1.0 - next_value)) (10)
            par.[i] <- r
            i <- i + 1
        __ret <- { _buffer = buf; _params = par; _time = (m._time) + 1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable aa: int = a
        let mutable bb: int = b
        let mutable res: int = 0
        let mutable bit: int = 1
        while (aa > 0) || (bb > 0) do
            let abit: int = ((aa % 2 + 2) % 2)
            let bbit: int = ((bb % 2 + 2) % 2)
            if abit <> bbit then
                res <- res + bit
            aa <- _floordiv aa 2
            bb <- _floordiv bb 2
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec xorshift (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable y = y
    try
        let mutable xv: int = x
        let mutable yv: int = y
        xv <- xor (xv) (_floordiv yv 8192)
        yv <- xor (yv) (xv * 131072)
        xv <- xor (xv) (_floordiv yv 32)
        __ret <- xv
        raise Return
        __ret
    with
        | Return -> __ret
let rec pull (m: Machine) =
    let mutable __ret : PullResult = Unchecked.defaultof<PullResult>
    let mutable m = m
    try
        let mutable buf: float array = m._buffer
        let mutable par: float array = m._params
        let key: int = (((m._time) % size + size) % size)
        let mutable i: int = 0
        while i < t do
            let mutable r: float = _idx par (key)
            let _value: float = _idx buf (key)
            buf.[key] <- round_dec ((r * _value) * (1.0 - _value)) (10)
            let mutable new_r: float = ((1.0 * (float (m._time))) * 0.01) + (r * 1.01)
            new_r <- new_r - (float (1.0 * (float (int (new_r)))))
            par.[key] <- new_r + 3.0
            i <- i + 1
        let x: int = int ((_idx buf ((((key + 2) % size + size) % size))) * 10000000000.0)
        let y: int = int ((_idx buf (((((key + size) - 2) % size + size) % size))) * 10000000000.0)
        let new_machine: Machine = { _buffer = buf; _params = par; _time = (m._time) + 1 }
        let _value: int64 = (((int64 (xorshift (x) (y))) % 4294967295L + 4294967295L) % 4294967295L)
        __ret <- { _value = int _value; _machine = new_machine }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable _machine: Machine = reset()
let mutable i: int = 0
while i < 100 do
    _machine <- push (_machine) (i)
    i <- i + 1
let mutable res: PullResult = pull (_machine)
printfn "%d" (res._value)
printfn "%s" (_repr ((res._machine)._buffer))
printfn "%s" (_repr ((res._machine)._params))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
