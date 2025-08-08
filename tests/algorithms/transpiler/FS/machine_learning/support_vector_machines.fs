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
type SVC = {
    mutable _weights: float array
    mutable _bias: float
    mutable _lr: float
    mutable _lambda: float
    mutable _epochs: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable s: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            s <- s + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_svc (_lr: float) (_lambda: float) (_epochs: int) =
    let mutable __ret : SVC = Unchecked.defaultof<SVC>
    let mutable _lr = _lr
    let mutable _lambda = _lambda
    let mutable _epochs = _epochs
    try
        __ret <- { _weights = Array.empty<float>; _bias = 0.0; _lr = _lr; _lambda = _lambda; _epochs = _epochs }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fit (model: SVC) (xs: float array array) (ys: int array) =
    let mutable __ret : SVC = Unchecked.defaultof<SVC>
    let mutable model = model
    let mutable xs = xs
    let mutable ys = ys
    try
        let n_features: int = Seq.length (_idx xs (0))
        let mutable w: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n_features do
            w <- Array.append w [|0.0|]
            i <- i + 1
        let mutable b: float = 0.0
        let mutable epoch: int = 0
        while epoch < (model._epochs) do
            let mutable j: int = 0
            while j < (Seq.length (xs)) do
                let x: float array = _idx xs (j)
                let y: float = float (_idx ys (j))
                let prod: float = (dot (w) (x)) + b
                if (y * prod) < 1.0 then
                    let mutable k: int = 0
                    while k < (Seq.length (w)) do
                        w.[k] <- (_idx w (k)) + ((model._lr) * ((y * (_idx x (k))) - ((2.0 * (model._lambda)) * (_idx w (k)))))
                        k <- k + 1
                    b <- b + ((model._lr) * y)
                else
                    let mutable k: int = 0
                    while k < (Seq.length (w)) do
                        w.[k] <- (_idx w (k)) - ((model._lr) * ((2.0 * (model._lambda)) * (_idx w (k))))
                        k <- k + 1
                j <- j + 1
            epoch <- epoch + 1
        __ret <- { _weights = w; _bias = b; _lr = model._lr; _lambda = model._lambda; _epochs = model._epochs }
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict (model: SVC) (x: float array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable model = model
    let mutable x = x
    try
        let mutable s: float = (dot (model._weights) (x)) + (model._bias)
        if s >= 0.0 then
            __ret <- 1
            raise Return
        else
            __ret <- -1
            raise Return
        __ret
    with
        | Return -> __ret
let xs: float array array = [|[|0.0; 1.0|]; [|0.0; 2.0|]; [|1.0; 1.0|]; [|1.0; 2.0|]|]
let ys: int array = unbox<int array> [|1; 1; -1; -1|]
let ``base``: SVC = new_svc (0.01) (0.01) (1000)
let model: SVC = fit (``base``) (xs) (ys)
printfn "%d" (predict (model) (unbox<float array> [|0.0; 1.0|]))
printfn "%d" (predict (model) (unbox<float array> [|1.0; 1.0|]))
printfn "%d" (predict (model) (unbox<float array> [|2.0; 2.0|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
