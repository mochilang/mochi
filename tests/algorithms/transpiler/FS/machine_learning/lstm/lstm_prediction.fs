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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type LSTMWeights = {
    mutable _w_i: float
    mutable _u_i: float
    mutable _b_i: float
    mutable _w_f: float
    mutable _u_f: float
    mutable _b_f: float
    mutable _w_o: float
    mutable _u_o: float
    mutable _b_o: float
    mutable _w_c: float
    mutable _u_c: float
    mutable _b_c: float
    mutable _w_y: float
    mutable _b_y: float
}
type LSTMState = {
    mutable _i: float array
    mutable _f: float array
    mutable _o: float array
    mutable _g: float array
    mutable _c: float array
    mutable _h: float array
}
type Samples = {
    mutable _x: float array array
    mutable _y: float array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec exp_approx (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * _x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sigmoid (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        __ret <- 1.0 / (1.0 + (exp_approx (-_x)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec tanh_approx (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        let e: float = exp_approx (2.0 * _x)
        __ret <- (e - 1.0) / (e + 1.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec forward (seq: float array) (w: LSTMWeights) =
    let mutable __ret : LSTMState = Unchecked.defaultof<LSTMState>
    let mutable seq = seq
    let mutable w = w
    try
        let mutable i_arr: float array = Array.empty<float>
        let mutable f_arr: float array = Array.empty<float>
        let mutable o_arr: float array = Array.empty<float>
        let mutable g_arr: float array = Array.empty<float>
        let mutable c_arr: float array = unbox<float array> [|0.0|]
        let mutable h_arr: float array = unbox<float array> [|0.0|]
        let mutable t: int = 0
        while t < (Seq.length (seq)) do
            let _x: float = _idx seq (t)
            let h_prev: float = _idx h_arr (t)
            let c_prev: float = _idx c_arr (t)
            let i_t: float = sigmoid ((((w._w_i) * _x) + ((w._u_i) * h_prev)) + (w._b_i))
            let f_t: float = sigmoid ((((w._w_f) * _x) + ((w._u_f) * h_prev)) + (w._b_f))
            let o_t: float = sigmoid ((((w._w_o) * _x) + ((w._u_o) * h_prev)) + (w._b_o))
            let g_t: float = tanh_approx ((((w._w_c) * _x) + ((w._u_c) * h_prev)) + (w._b_c))
            let c_t: float = (f_t * c_prev) + (i_t * g_t)
            let h_t: float = o_t * (tanh_approx (c_t))
            i_arr <- Array.append i_arr [|i_t|]
            f_arr <- Array.append f_arr [|f_t|]
            o_arr <- Array.append o_arr [|o_t|]
            g_arr <- Array.append g_arr [|g_t|]
            c_arr <- Array.append c_arr [|c_t|]
            h_arr <- Array.append h_arr [|h_t|]
            t <- t + 1
        __ret <- { _i = i_arr; _f = f_arr; _o = o_arr; _g = g_arr; _c = c_arr; _h = h_arr }
        raise Return
        __ret
    with
        | Return -> __ret
let rec backward (seq: float array) (target: float) (w: LSTMWeights) (s: LSTMState) (lr: float) =
    let mutable __ret : LSTMWeights = Unchecked.defaultof<LSTMWeights>
    let mutable seq = seq
    let mutable target = target
    let mutable w = w
    let mutable s = s
    let mutable lr = lr
    try
        let mutable dw_i: float = 0.0
        let mutable du_i: float = 0.0
        let mutable db_i: float = 0.0
        let mutable dw_f: float = 0.0
        let mutable du_f: float = 0.0
        let mutable db_f: float = 0.0
        let mutable dw_o: float = 0.0
        let mutable du_o: float = 0.0
        let mutable db_o: float = 0.0
        let mutable dw_c: float = 0.0
        let mutable du_c: float = 0.0
        let mutable db_c: float = 0.0
        let mutable dw_y: float = 0.0
        let mutable db_y: float = 0.0
        let T: int = Seq.length (seq)
        let h_last: float = _idx (s._h) (T)
        let _y: float = ((w._w_y) * h_last) + (w._b_y)
        let dy: float = _y - target
        dw_y <- dy * h_last
        db_y <- dy
        let mutable dh_next: float = dy * (w._w_y)
        let mutable dc_next: float = 0.0
        let mutable t: int = T - 1
        while t >= 0 do
            let i_t: float = _idx (s._i) (t)
            let f_t: float = _idx (s._f) (t)
            let o_t: float = _idx (s._o) (t)
            let g_t: float = _idx (s._g) (t)
            let c_t: float = _idx (s._c) (t + 1)
            let c_prev: float = _idx (s._c) (t)
            let h_prev: float = _idx (s._h) (t)
            let tanh_c: float = tanh_approx (c_t)
            let do_t: float = dh_next * tanh_c
            let da_o: float = (do_t * o_t) * (1.0 - o_t)
            let dc: float = ((dh_next * o_t) * (1.0 - (tanh_c * tanh_c))) + dc_next
            let di_t: float = dc * g_t
            let da_i: float = (di_t * i_t) * (1.0 - i_t)
            let dg_t: float = dc * i_t
            let da_g: float = dg_t * (1.0 - (g_t * g_t))
            let df_t: float = dc * c_prev
            let da_f: float = (df_t * f_t) * (1.0 - f_t)
            dw_i <- dw_i + (da_i * (_idx seq (t)))
            du_i <- du_i + (da_i * h_prev)
            db_i <- db_i + da_i
            dw_f <- dw_f + (da_f * (_idx seq (t)))
            du_f <- du_f + (da_f * h_prev)
            db_f <- db_f + da_f
            dw_o <- dw_o + (da_o * (_idx seq (t)))
            du_o <- du_o + (da_o * h_prev)
            db_o <- db_o + da_o
            dw_c <- dw_c + (da_g * (_idx seq (t)))
            du_c <- du_c + (da_g * h_prev)
            db_c <- db_c + da_g
            dh_next <- (((da_i * (w._u_i)) + (da_f * (w._u_f))) + (da_o * (w._u_o))) + (da_g * (w._u_c))
            dc_next <- dc * f_t
            t <- t - 1
        w._w_y <- (w._w_y) - (lr * dw_y)
        w._b_y <- (w._b_y) - (lr * db_y)
        w._w_i <- (w._w_i) - (lr * dw_i)
        w._u_i <- (w._u_i) - (lr * du_i)
        w._b_i <- (w._b_i) - (lr * db_i)
        w._w_f <- (w._w_f) - (lr * dw_f)
        w._u_f <- (w._u_f) - (lr * du_f)
        w._b_f <- (w._b_f) - (lr * db_f)
        w._w_o <- (w._w_o) - (lr * dw_o)
        w._u_o <- (w._u_o) - (lr * du_o)
        w._b_o <- (w._b_o) - (lr * db_o)
        w._w_c <- (w._w_c) - (lr * dw_c)
        w._u_c <- (w._u_c) - (lr * du_c)
        w._b_c <- (w._b_c) - (lr * db_c)
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_samples (data: float array) (look_back: int) =
    let mutable __ret : Samples = Unchecked.defaultof<Samples>
    let mutable data = data
    let mutable look_back = look_back
    try
        let mutable X: float array array = Array.empty<float array>
        let mutable Y: float array = Array.empty<float>
        let mutable _i: int = 0
        while (_i + look_back) < (Seq.length (data)) do
            let seq: float array = Array.sub data _i ((_i + look_back) - _i)
            X <- Array.append X [|seq|]
            Y <- Array.append Y [|(_idx data (_i + look_back))|]
            _i <- _i + 1
        __ret <- { _x = X; _y = Y }
        raise Return
        __ret
    with
        | Return -> __ret
let rec init_weights () =
    let mutable __ret : LSTMWeights = Unchecked.defaultof<LSTMWeights>
    try
        __ret <- { _w_i = 0.1; _u_i = 0.2; _b_i = 0.0; _w_f = 0.1; _u_f = 0.2; _b_f = 0.0; _w_o = 0.1; _u_o = 0.2; _b_o = 0.0; _w_c = 0.1; _u_c = 0.2; _b_c = 0.0; _w_y = 0.1; _b_y = 0.0 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec train (data: float array) (look_back: int) (epochs: int) (lr: float) =
    let mutable __ret : LSTMWeights = Unchecked.defaultof<LSTMWeights>
    let mutable data = data
    let mutable look_back = look_back
    let mutable epochs = epochs
    let mutable lr = lr
    try
        let samples: Samples = make_samples (data) (look_back)
        let mutable w: LSTMWeights = init_weights()
        let mutable ep: int = 0
        while ep < epochs do
            let mutable j: int = 0
            while j < (Seq.length (samples._x)) do
                let seq: float array = _idx (samples._x) (j)
                let target: float = _idx (samples._y) (j)
                let state: LSTMState = forward (seq) (w)
                w <- backward (seq) (target) (w) (state) (lr)
                j <- j + 1
            ep <- ep + 1
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict (seq: float array) (w: LSTMWeights) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable seq = seq
    let mutable w = w
    try
        let state: LSTMState = forward (seq) (w)
        let h_last: float = _idx (state._h) ((Seq.length (state._h)) - 1)
        __ret <- ((w._w_y) * h_last) + (w._b_y)
        raise Return
        __ret
    with
        | Return -> __ret
let data: float array = unbox<float array> [|0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8|]
let look_back: int = 3
let epochs: int = 200
let lr: float = 0.1
let mutable w: LSTMWeights = train (data) (look_back) (epochs) (lr)
let test_seq: float array = unbox<float array> [|0.6; 0.7; 0.8|]
let pred: float = predict (test_seq) (w)
printfn "%s" ("Predicted value: " + (_str (pred)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
