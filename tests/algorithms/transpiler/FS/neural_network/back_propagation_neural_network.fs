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
type Layer = {
    mutable _units: int
    mutable _weight: float array array
    mutable _bias: float array
    mutable _output: float array
    mutable _xdata: float array
    mutable _learn_rate: float
}
type Data = {
    mutable _x: float array array
    mutable _y: float array array
}
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and expApprox (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let mutable _y: float = _x
        let mutable is_neg: bool = false
        if _x < 0.0 then
            is_neg <- true
            _y <- -_x
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 30 do
            term <- (term * _y) / (float n)
            sum <- sum + term
            n <- n + 1
        if is_neg then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid (z: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable z = z
    try
        __ret <- 1.0 / (1.0 + (expApprox (-z)))
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid_vec (v: float array) =
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
and sigmoid_derivative (out: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable out = out
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (out)) do
            let ``val``: float = _idx out (int i)
            res <- Array.append res [|(``val`` * (1.0 - ``val``))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and random_vector (n: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable n = n
    try
        let mutable v: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            v <- Array.append v [|((random()) - 0.5)|]
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
and random_matrix (r: int) (c: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable r = r
    let mutable c = c
    try
        let mutable m: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < r do
            m <- Array.append m [|(random_vector (c))|]
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and matvec (mat: float array array) (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mat = mat
    let mutable vec = vec
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable s: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (vec)) do
                s <- s + ((_idx (_idx mat (int i)) (int j)) * (_idx vec (int j)))
                j <- j + 1
            res <- Array.append res [|s|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matTvec (mat: float array array) (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mat = mat
    let mutable vec = vec
    try
        let mutable cols: int = Seq.length (_idx mat (int 0))
        let mutable res: float array = Array.empty<float>
        let mutable j: int = 0
        while j < cols do
            let mutable s: float = 0.0
            let mutable i: int = 0
            while i < (Seq.length (mat)) do
                s <- s + ((_idx (_idx mat (int i)) (int j)) * (_idx vec (int i)))
                i <- i + 1
            res <- Array.append res [|s|]
            j <- j + 1
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
and vec_scalar_mul (v: float array) (s: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable v = v
    let mutable s = s
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|((_idx v (int i)) * s)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and outer (a: float array) (b: float array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (b)) do
                row <- Array.append row [|((_idx a (int i)) * (_idx b (int j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and mat_scalar_mul (mat: float array array) (s: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    let mutable s = s
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (int i))) do
                row <- Array.append row [|((_idx (_idx mat (int i)) (int j)) * s)|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and mat_sub (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx a (int i))) do
                row <- Array.append row [|((_idx (_idx a (int i)) (int j)) - (_idx (_idx b (int i)) (int j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and init_layer (_units: int) (back_units: int) (lr: float) =
    let mutable __ret : Layer = Unchecked.defaultof<Layer>
    let mutable _units = _units
    let mutable back_units = back_units
    let mutable lr = lr
    try
        __ret <- { _units = _units; _weight = random_matrix (_units) (back_units); _bias = random_vector (_units); _output = Array.empty<float>; _xdata = Array.empty<float>; _learn_rate = lr }
        raise Return
        __ret
    with
        | Return -> __ret
and forward (layers: Layer array) (_x: float array) =
    let mutable __ret : Layer array = Unchecked.defaultof<Layer array>
    let mutable layers = layers
    let mutable _x = _x
    try
        let mutable data: float array = _x
        let mutable i: int = 0
        while i < (Seq.length (layers)) do
            let mutable layer: Layer = _idx layers (int i)
            layer._xdata <- data
            if i = 0 then
                layer._output <- data
            else
                let z: float array = vec_sub (matvec (layer._weight) (data)) (layer._bias)
                layer._output <- sigmoid_vec (z)
                data <- layer._output
            layers.[int i] <- layer
            i <- i + 1
        __ret <- layers
        raise Return
        __ret
    with
        | Return -> __ret
and backward (layers: Layer array) (grad: float array) =
    let mutable __ret : Layer array = Unchecked.defaultof<Layer array>
    let mutable layers = layers
    let mutable grad = grad
    try
        let mutable g: float array = grad
        let mutable i: int = (Seq.length (layers)) - 1
        while i > 0 do
            let mutable layer: Layer = _idx layers (int i)
            let deriv: float array = sigmoid_derivative (layer._output)
            let delta: float array = vec_mul (g) (deriv)
            let grad_w: float array array = outer (delta) (layer._xdata)
            layer._weight <- mat_sub (layer._weight) (mat_scalar_mul (grad_w) (layer._learn_rate))
            layer._bias <- vec_sub (layer._bias) (vec_scalar_mul (delta) (layer._learn_rate))
            g <- matTvec (layer._weight) (delta)
            layers.[int i] <- layer
            i <- i - 1
        __ret <- layers
        raise Return
        __ret
    with
        | Return -> __ret
and calc_loss (_y: float array) (yhat: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _y = _y
    let mutable yhat = yhat
    try
        let mutable s: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (_y)) do
            let d: float = (_idx _y (int i)) - (_idx yhat (int i))
            s <- s + (d * d)
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and calc_gradient (_y: float array) (yhat: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable _y = _y
    let mutable yhat = yhat
    try
        let mutable g: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (_y)) do
            g <- Array.append g [|(2.0 * ((_idx yhat (int i)) - (_idx _y (int i))))|]
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and train (layers: Layer array) (_xdata: float array array) (ydata: float array array) (rounds: int) (acc: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable layers = layers
    let mutable _xdata = _xdata
    let mutable ydata = ydata
    let mutable rounds = rounds
    let mutable acc = acc
    try
        let mutable r: int = 0
        while r < rounds do
            let mutable i: int = 0
            while i < (Seq.length (_xdata)) do
                layers <- forward (layers) (_idx _xdata (int i))
                let out: float array = (_idx layers (int ((Seq.length (layers)) - 1)))._output
                let grad: float array = calc_gradient (_idx ydata (int i)) (out)
                layers <- backward (layers) (grad)
                i <- i + 1
            r <- r + 1
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and create_data () =
    let mutable __ret : Data = Unchecked.defaultof<Data>
    try
        let mutable _x: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < 10 do
            _x <- Array.append _x [|(random_vector (10))|]
            i <- i + 1
        let mutable _y: float array array = [|[|0.8; 0.4|]; [|0.4; 0.3|]; [|0.34; 0.45|]; [|0.67; 0.32|]; [|0.88; 0.67|]; [|0.78; 0.77|]; [|0.55; 0.66|]; [|0.55; 0.43|]; [|0.54; 0.1|]; [|0.1; 0.5|]|]
        __ret <- { _x = _x; _y = _y }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable data: Data = create_data()
        let mutable _x: float array array = data._x
        let mutable _y: float array array = data._y
        let mutable layers: Layer array = Array.empty<Layer>
        layers <- Array.append layers [|(init_layer (10) (0) (0.3))|]
        layers <- Array.append layers [|(init_layer (20) (10) (0.3))|]
        layers <- Array.append layers [|(init_layer (30) (20) (0.3))|]
        layers <- Array.append layers [|(init_layer (2) (30) (0.3))|]
        let final_mse: float = train (layers) (_x) (_y) (100) (0.01)
        printfn "%g" (final_mse)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
