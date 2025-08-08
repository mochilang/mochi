// Generated 2025-08-08 17:07 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- sum + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec maxf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec minf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then x else (0.0 - x)
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict_raw (samples: float array array) (labels: float array) (alphas: float array) (b: float) (x: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable samples = samples
    let mutable labels = labels
    let mutable alphas = alphas
    let mutable b = b
    let mutable x = x
    try
        let mutable res: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (samples)) do
            res <- res + (((_idx alphas (i)) * (_idx labels (i))) * (dot (_idx samples (i)) (x)))
            i <- i + 1
        __ret <- res + b
        raise Return
        __ret
    with
        | Return -> __ret
let rec smo_train (samples: float array array) (labels: float array) (c: float) (tol: float) (max_passes: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable samples = samples
    let mutable labels = labels
    let mutable c = c
    let mutable tol = tol
    let mutable max_passes = max_passes
    try
        let m: int = Seq.length (samples)
        let mutable alphas: float array = Array.empty<float>
        let mutable i: int = 0
        while i < m do
            alphas <- Array.append alphas [|0.0|]
            i <- i + 1
        let mutable b: float = 0.0
        let mutable passes: int = 0
        try
            while passes < max_passes do
                try
                    let mutable num_changed: int = 0
                    let mutable i1: int = 0
                    try
                        while i1 < m do
                            try
                                let Ei: float = (predict_raw (samples) (labels) (alphas) (b) (_idx samples (i1))) - (_idx labels (i1))
                                if ((((_idx labels (i1)) * Ei) < (0.0 - tol)) && ((_idx alphas (i1)) < c)) || ((((_idx labels (i1)) * Ei) > tol) && ((_idx alphas (i1)) > 0.0)) then
                                    let mutable i2: int = (((i1 + 1) % m + m) % m)
                                    let Ej: float = (predict_raw (samples) (labels) (alphas) (b) (_idx samples (i2))) - (_idx labels (i2))
                                    let alpha1_old: float = _idx alphas (i1)
                                    let alpha2_old: float = _idx alphas (i2)
                                    let mutable L: float = 0.0
                                    let mutable H: float = 0.0
                                    if (_idx labels (i1)) <> (_idx labels (i2)) then
                                        L <- maxf (0.0) (alpha2_old - alpha1_old)
                                        H <- minf (c) ((c + alpha2_old) - alpha1_old)
                                    else
                                        L <- maxf (0.0) ((alpha2_old + alpha1_old) - c)
                                        H <- minf (c) (alpha2_old + alpha1_old)
                                    if L = H then
                                        i1 <- i1 + 1
                                        raise Continue
                                    let eta: float = ((2.0 * (dot (_idx samples (i1)) (_idx samples (i2)))) - (dot (_idx samples (i1)) (_idx samples (i1)))) - (dot (_idx samples (i2)) (_idx samples (i2)))
                                    if eta >= 0.0 then
                                        i1 <- i1 + 1
                                        raise Continue
                                    alphas.[i2] <- alpha2_old - (((_idx labels (i2)) * (Ei - Ej)) / eta)
                                    if (_idx alphas (i2)) > H then
                                        alphas.[i2] <- H
                                    if (_idx alphas (i2)) < L then
                                        alphas.[i2] <- L
                                    if (absf ((_idx alphas (i2)) - alpha2_old)) < 0.00001 then
                                        i1 <- i1 + 1
                                        raise Continue
                                    alphas.[i1] <- alpha1_old + (((_idx labels (i1)) * (_idx labels (i2))) * (alpha2_old - (_idx alphas (i2))))
                                    let b1: float = ((b - Ei) - (((_idx labels (i1)) * ((_idx alphas (i1)) - alpha1_old)) * (dot (_idx samples (i1)) (_idx samples (i1))))) - (((_idx labels (i2)) * ((_idx alphas (i2)) - alpha2_old)) * (dot (_idx samples (i1)) (_idx samples (i2))))
                                    let b2: float = ((b - Ej) - (((_idx labels (i1)) * ((_idx alphas (i1)) - alpha1_old)) * (dot (_idx samples (i1)) (_idx samples (i2))))) - (((_idx labels (i2)) * ((_idx alphas (i2)) - alpha2_old)) * (dot (_idx samples (i2)) (_idx samples (i2))))
                                    if ((_idx alphas (i1)) > 0.0) && ((_idx alphas (i1)) < c) then
                                        b <- b1
                                    else
                                        if ((_idx alphas (i2)) > 0.0) && ((_idx alphas (i2)) < c) then
                                            b <- b2
                                        else
                                            b <- (b1 + b2) / 2.0
                                    num_changed <- num_changed + 1
                                i1 <- i1 + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if num_changed = 0 then
                        passes <- passes + 1
                    else
                        passes <- 0
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- [|alphas; [|b|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict (samples: float array array) (labels: float array) (model: float array array) (x: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable samples = samples
    let mutable labels = labels
    let mutable model = model
    let mutable x = x
    try
        let mutable alphas: float array = _idx model (0)
        let mutable b: float = _idx (_idx model (1)) (0)
        let ``val``: float = predict_raw (samples) (labels) (alphas) (b) (x)
        if ``val`` >= 0.0 then
            __ret <- 1.0
            raise Return
        __ret <- -1.0
        raise Return
        __ret
    with
        | Return -> __ret
let samples: float array array = [|[|2.0; 2.0|]; [|1.5; 1.5|]; [|0.0; 0.0|]; [|0.5; 0.0|]|]
let labels: float array = unbox<float array> [|1.0; 1.0; -1.0; -1.0|]
let model: float array array = smo_train (samples) (labels) (1.0) (0.001) (10)
printfn "%g" (predict (samples) (labels) (model) (unbox<float array> [|1.5; 1.0|]))
printfn "%g" (predict (samples) (labels) (model) (unbox<float array> [|0.2; 0.1|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
