// Generated 2025-08-12 08:17 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let rec key (x: int) (y: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable y = y
    try
        __ret <- ((_str (x)) + ",") + (_str (y))
        raise Return
        __ret
    with
        | Return -> __ret
and joint_probability_distribution (x_values: int array) (y_values: int array) (x_probabilities: float array) (y_probabilities: float array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable x_values = x_values
    let mutable y_values = y_values
    let mutable x_probabilities = x_probabilities
    let mutable y_probabilities = y_probabilities
    try
        let mutable result: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        let mutable i: int = 0
        while i < (Seq.length (x_values)) do
            let mutable j: int = 0
            while j < (Seq.length (y_values)) do
                let k: string = key (_idx x_values (int i)) (_idx y_values (int j))
                result <- _dictAdd (result) (string (k)) ((_idx x_probabilities (int i)) * (_idx y_probabilities (int j)))
                j <- j + 1
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and expectation (values: int array) (probabilities: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable values = values
    let mutable probabilities = probabilities
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            total <- total + ((float (_idx values (int i))) * (_idx probabilities (int i)))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and variance (values: int array) (probabilities: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable values = values
    let mutable probabilities = probabilities
    try
        let mean: float = expectation (values) (probabilities)
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            let diff: float = (float (_idx values (int i))) - mean
            total <- total + ((diff * diff) * (_idx probabilities (int i)))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and covariance (x_values: int array) (y_values: int array) (x_probabilities: float array) (y_probabilities: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x_values = x_values
    let mutable y_values = y_values
    let mutable x_probabilities = x_probabilities
    let mutable y_probabilities = y_probabilities
    try
        let mean_x: float = expectation (x_values) (x_probabilities)
        let mean_y: float = expectation (y_values) (y_probabilities)
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (x_values)) do
            let mutable j: int = 0
            while j < (Seq.length (y_values)) do
                let diff_x: float = (float (_idx x_values (int i))) - mean_x
                let diff_y: float = (float (_idx y_values (int j))) - mean_y
                total <- total + (((diff_x * diff_y) * (_idx x_probabilities (int i))) * (_idx y_probabilities (int j)))
                j <- j + 1
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and standard_deviation (v: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        __ret <- sqrtApprox (v)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let x_values: int array = unbox<int array> [|1; 2|]
        let y_values: int array = unbox<int array> [|-2; 5; 8|]
        let x_probabilities: float array = unbox<float array> [|0.7; 0.3|]
        let y_probabilities: float array = unbox<float array> [|0.3; 0.5; 0.2|]
        let jpd: System.Collections.Generic.IDictionary<string, float> = joint_probability_distribution (x_values) (y_values) (x_probabilities) (y_probabilities)
        let mutable i: int = 0
        while i < (Seq.length (x_values)) do
            let mutable j: int = 0
            while j < (Seq.length (y_values)) do
                let k: string = key (_idx x_values (int i)) (_idx y_values (int j))
                let prob: float = _dictGet jpd ((string (k)))
                printfn "%s" ((k + "=") + (_str (prob)))
                j <- j + 1
            i <- i + 1
        let ex: float = expectation (x_values) (x_probabilities)
        let ey: float = expectation (y_values) (y_probabilities)
        let vx: float = variance (x_values) (x_probabilities)
        let vy: float = variance (y_values) (y_probabilities)
        let cov: float = covariance (x_values) (y_values) (x_probabilities) (y_probabilities)
        printfn "%s" ("Ex=" + (_str (ex)))
        printfn "%s" ("Ey=" + (_str (ey)))
        printfn "%s" ("Vx=" + (_str (vx)))
        printfn "%s" ("Vy=" + (_str (vy)))
        printfn "%s" ("Cov=" + (_str (cov)))
        printfn "%s" ("Sx=" + (_str (standard_deviation (vx))))
        printfn "%s" ("Sy=" + (_str (standard_deviation (vy))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
