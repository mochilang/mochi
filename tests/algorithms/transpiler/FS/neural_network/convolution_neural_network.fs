// Generated 2025-08-09 10:14 +0700

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
type CNN = {
    mutable _conv_kernels: float array array array
    mutable _conv_bias: float array
    mutable _conv_step: int
    mutable _pool_size: int
    mutable _w_hidden: float array array
    mutable _w_out: float array array
    mutable _b_hidden: float array
    mutable _b_out: float array
    mutable _rate_weight: float
    mutable _rate_bias: float
}
type TrainSample = {
    mutable _image: float array array
    mutable _target: float array
}
let mutable _seed: int = 1
let rec random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        _seed <- int ((((((int64 _seed) * (int64 13)) + (int64 7)) % (int64 100) + (int64 100)) % (int64 100)))
        __ret <- (float _seed) / 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0 / (1.0 + (exp (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
and to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and exp (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * x) / (to_float (n))
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and convolve (data: float array array) (kernel: float array array) (step: int) (bias: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable data = data
    let mutable kernel = kernel
    let mutable step = step
    let mutable bias = bias
    try
        let size_data: int = Seq.length (data)
        let size_kernel: int = Seq.length (kernel)
        let mutable out: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i <= (size_data - size_kernel) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j <= (size_data - size_kernel) do
                let mutable sum: float = 0.0
                let mutable a: int = 0
                while a < size_kernel do
                    let mutable b: int = 0
                    while b < size_kernel do
                        sum <- sum + ((_idx (_idx data (int (i + a))) (int (j + b))) * (_idx (_idx kernel (int a)) (int b)))
                        b <- b + 1
                    a <- a + 1
                row <- Array.append row [|(sigmoid (sum - bias))|]
                j <- j + step
            out <- Array.append out [|row|]
            i <- i + step
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and average_pool (map: float array array) (size: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable map = map
    let mutable size = size
    try
        let mutable out: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (map)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx map (int i))) do
                let mutable sum: float = 0.0
                let mutable a: int = 0
                while a < size do
                    let mutable b: int = 0
                    while b < size do
                        sum <- sum + (_idx (_idx map (int (i + a))) (int (j + b)))
                        b <- b + 1
                    a <- a + 1
                row <- Array.append row [|(sum / (float ((int64 size) * (int64 size))))|]
                j <- j + size
            out <- Array.append out [|row|]
            i <- i + size
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and flatten (maps: float array array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable maps = maps
    try
        let mutable out: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (maps)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx maps (int i))) do
                let mutable k: int = 0
                while k < (Seq.length (_idx (_idx maps (int i)) (int j))) do
                    out <- Array.append out [|(_idx (_idx (_idx maps (int i)) (int j)) (int k))|]
                    k <- k + 1
                j <- j + 1
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and vec_mul_mat (v: float array) (m: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable v = v
    let mutable m = m
    try
        let mutable cols: int = Seq.length (_idx m (int 0))
        let mutable res: float array = Array.empty<float>
        let mutable j: int = 0
        while j < cols do
            let mutable sum: float = 0.0
            let mutable i: int = 0
            while i < (Seq.length (v)) do
                sum <- sum + ((_idx v (int i)) * (_idx (_idx m (int i)) (int j)))
                i <- i + 1
            res <- Array.append res [|sum|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matT_vec_mul (m: float array array) (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable m = m
    let mutable v = v
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_idx m (int i))) do
                sum <- sum + ((_idx (_idx m (int i)) (int j)) * (_idx v (int j)))
                j <- j + 1
            res <- Array.append res [|sum|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and vec_add (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (int i)) + (_idx b (int i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and vec_sub (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (int i)) - (_idx b (int i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and vec_mul (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (int i)) * (_idx b (int i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and vec_map_sig (v: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable v = v
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|(sigmoid (_idx v (int i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and new_cnn () =
    let mutable __ret : CNN = Unchecked.defaultof<CNN>
    try
        let k1: float array array = [|[|1.0; 0.0|]; [|0.0; 1.0|]|]
        let k2: float array array = [|[|0.0; 1.0|]; [|1.0; 0.0|]|]
        let _conv_kernels: float array array array = [|k1; k2|]
        let _conv_bias: float array = unbox<float array> [|0.0; 0.0|]
        let _conv_step: int = 2
        let _pool_size: int = 2
        let input_size: int = 2
        let hidden_size: int = 2
        let output_size: int = 2
        let mutable _w_hidden: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < input_size do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < hidden_size do
                row <- Array.append row [|((random()) - 0.5)|]
                j <- j + 1
            _w_hidden <- Array.append _w_hidden [|row|]
            i <- i + 1
        let mutable _w_out: float array array = Array.empty<float array>
        i <- 0
        while i < hidden_size do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < output_size do
                row <- Array.append row [|((random()) - 0.5)|]
                j <- j + 1
            _w_out <- Array.append _w_out [|row|]
            i <- i + 1
        let _b_hidden: float array = unbox<float array> [|0.0; 0.0|]
        let _b_out: float array = unbox<float array> [|0.0; 0.0|]
        __ret <- { _conv_kernels = _conv_kernels; _conv_bias = _conv_bias; _conv_step = _conv_step; _pool_size = _pool_size; _w_hidden = _w_hidden; _w_out = _w_out; _b_hidden = _b_hidden; _b_out = _b_out; _rate_weight = 0.2; _rate_bias = 0.2 }
        raise Return
        __ret
    with
        | Return -> __ret
and forward (cnn: CNN) (data: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable cnn = cnn
    let mutable data = data
    try
        let mutable maps: float array array array = Array.empty<float array array>
        let mutable i: int = 0
        while i < (Seq.length (cnn._conv_kernels)) do
            let conv_map: float array array = convolve (data) (_idx (cnn._conv_kernels) (int i)) (cnn._conv_step) (_idx (cnn._conv_bias) (int i))
            let pooled: float array array = average_pool (conv_map) (cnn._pool_size)
            maps <- Array.append maps [|pooled|]
            i <- i + 1
        let flat: float array = flatten (maps)
        let hidden_net: float array = vec_add (vec_mul_mat (flat) (cnn._w_hidden)) (cnn._b_hidden)
        let hidden_out: float array = vec_map_sig (hidden_net)
        let out_net: float array = vec_add (vec_mul_mat (hidden_out) (cnn._w_out)) (cnn._b_out)
        let mutable out: float array = vec_map_sig (out_net)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and train (cnn: CNN) (samples: TrainSample array) (epochs: int) =
    let mutable __ret : CNN = Unchecked.defaultof<CNN>
    let mutable cnn = cnn
    let mutable samples = samples
    let mutable epochs = epochs
    try
        let mutable _w_out: float array array = cnn._w_out
        let mutable _b_out: float array = cnn._b_out
        let mutable _w_hidden: float array array = cnn._w_hidden
        let mutable _b_hidden: float array = cnn._b_hidden
        let mutable e: int = 0
        while e < epochs do
            let mutable s: int = 0
            while s < (Seq.length (samples)) do
                let data: float array array = (_idx samples (int s))._image
                let _target: float array = (_idx samples (int s))._target
                let mutable maps: float array array array = Array.empty<float array array>
                let mutable i: int = 0
                while i < (Seq.length (cnn._conv_kernels)) do
                    let conv_map: float array array = convolve (data) (_idx (cnn._conv_kernels) (int i)) (cnn._conv_step) (_idx (cnn._conv_bias) (int i))
                    let pooled: float array array = average_pool (conv_map) (cnn._pool_size)
                    maps <- Array.append maps [|pooled|]
                    i <- i + 1
                let flat: float array = flatten (maps)
                let hidden_net: float array = vec_add (vec_mul_mat (flat) (_w_hidden)) (_b_hidden)
                let hidden_out: float array = vec_map_sig (hidden_net)
                let out_net: float array = vec_add (vec_mul_mat (hidden_out) (_w_out)) (_b_out)
                let mutable out: float array = vec_map_sig (out_net)
                let error_out: float array = vec_sub (_target) (out)
                let pd_out: float array = vec_mul (error_out) (vec_mul (out) (vec_sub (unbox<float array> [|1.0; 1.0|]) (out)))
                let error_hidden: float array = matT_vec_mul (_w_out) (pd_out)
                let pd_hidden: float array = vec_mul (error_hidden) (vec_mul (hidden_out) (vec_sub (unbox<float array> [|1.0; 1.0|]) (hidden_out)))
                let mutable j: int = 0
                while j < (Seq.length (_w_out)) do
                    let mutable k: int = 0
                    while k < (Seq.length (_idx _w_out (int j))) do
                        _w_out.[int j].[int k] <- (_idx (_idx _w_out (int j)) (int k)) + (((cnn._rate_weight) * (_idx hidden_out (int j))) * (_idx pd_out (int k)))
                        k <- k + 1
                    j <- j + 1
                j <- 0
                while j < (Seq.length (_b_out)) do
                    _b_out.[int j] <- (_idx _b_out (int j)) - ((cnn._rate_bias) * (_idx pd_out (int j)))
                    j <- j + 1
                let mutable i_h: int = 0
                while i_h < (Seq.length (_w_hidden)) do
                    let mutable j_h: int = 0
                    while j_h < (Seq.length (_idx _w_hidden (int i_h))) do
                        _w_hidden.[int i_h].[int j_h] <- (_idx (_idx _w_hidden (int i_h)) (int j_h)) + (((cnn._rate_weight) * (_idx flat (int i_h))) * (_idx pd_hidden (int j_h)))
                        j_h <- j_h + 1
                    i_h <- i_h + 1
                j <- 0
                while j < (Seq.length (_b_hidden)) do
                    _b_hidden.[int j] <- (_idx _b_hidden (int j)) - ((cnn._rate_bias) * (_idx pd_hidden (int j)))
                    j <- j + 1
                s <- s + 1
            e <- e + 1
        __ret <- { _conv_kernels = cnn._conv_kernels; _conv_bias = cnn._conv_bias; _conv_step = cnn._conv_step; _pool_size = cnn._pool_size; _w_hidden = _w_hidden; _w_out = _w_out; _b_hidden = _b_hidden; _b_out = _b_out; _rate_weight = cnn._rate_weight; _rate_bias = cnn._rate_bias }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let cnn: CNN = new_cnn()
        let _image: float array array = [|[|1.0; 0.0; 1.0; 0.0|]; [|0.0; 1.0; 0.0; 1.0|]; [|1.0; 0.0; 1.0; 0.0|]; [|0.0; 1.0; 0.0; 1.0|]|]
        let sample: TrainSample = { _image = _image; _target = unbox<float array> [|1.0; 0.0|] }
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Before training:"); sprintf "%A" (forward (cnn) (_image))|]))
        let trained: CNN = train (cnn) (unbox<TrainSample array> [|sample|]) (50)
        printfn "%s" (String.concat " " ([|sprintf "%s" ("After training:"); sprintf "%A" (forward (trained) (_image))|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
