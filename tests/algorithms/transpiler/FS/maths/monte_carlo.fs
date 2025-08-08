// Generated 2025-08-08 18:09 +0700

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
let PI: float = 3.141592653589793
let mutable rand_seed: int = 123456789
let rec rand_float () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        rand_seed <- int ((((((int64 1103515245) * (int64 rand_seed)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- (float rand_seed) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and rand_range (min_val: float) (max_val: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable min_val = min_val
    let mutable max_val = max_val
    try
        __ret <- ((rand_float()) * (max_val - min_val)) + min_val
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x = 0.0 then
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
and pi_estimator (iterations: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable iterations = iterations
    try
        let mutable inside: float = 0.0
        let mutable i: int = 0
        while i < iterations do
            let x: float = rand_range (-1.0) (1.0)
            let y: float = rand_range (-1.0) (1.0)
            if ((x * x) + (y * y)) <= 1.0 then
                inside <- inside + 1.0
            i <- i + 1
        let proportion: float = inside / (float iterations)
        let pi_estimate: float = proportion * 4.0
        printfn "%s" (String.concat " " ([|sprintf "%s" ("The estimated value of pi is"); sprintf "%g" (pi_estimate)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("The numpy value of pi is"); sprintf "%g" (PI)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("The total error is"); sprintf "%g" (abs_float (PI - pi_estimate))|]))
        __ret
    with
        | Return -> __ret
and area_under_curve_estimator (iterations: int) (f: float -> float) (min_value: float) (max_value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable iterations = iterations
    let mutable f = f
    let mutable min_value = min_value
    let mutable max_value = max_value
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < iterations do
            let x: float = rand_range (min_value) (max_value)
            sum <- sum + (float (f (x)))
            i <- i + 1
        let expected: float = sum / (float iterations)
        __ret <- expected * (max_value - min_value)
        raise Return
        __ret
    with
        | Return -> __ret
and area_under_line_estimator_check (iterations: int) (min_value: float) (max_value: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable iterations = iterations
    let mutable min_value = min_value
    let mutable max_value = max_value
    try
        let rec identity_function (x: float) =
            let mutable __ret : float = Unchecked.defaultof<float>
            let mutable x = x
            try
                __ret <- x
                raise Return
                __ret
            with
                | Return -> __ret
        let estimated_value: float = area_under_curve_estimator (iterations) (unbox<float -> float> identity_function) (min_value) (max_value)
        let expected_value: float = ((max_value * max_value) - (min_value * min_value)) / 2.0
        printfn "%s" ("******************")
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Estimating area under y=x where x varies from"); sprintf "%g" (min_value)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Estimated value is"); sprintf "%g" (estimated_value)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Expected value is"); sprintf "%g" (expected_value)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Total error is"); sprintf "%g" (abs_float (estimated_value - expected_value))|]))
        printfn "%s" ("******************")
        __ret
    with
        | Return -> __ret
and pi_estimator_using_area_under_curve (iterations: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable iterations = iterations
    try
        let rec semi_circle (x: float) =
            let mutable __ret : float = Unchecked.defaultof<float>
            let mutable x = x
            try
                let y: float = 4.0 - (x * x)
                let s: float = sqrtApprox (y)
                __ret <- s
                raise Return
                __ret
            with
                | Return -> __ret
        let estimated_value: float = area_under_curve_estimator (iterations) (unbox<float -> float> semi_circle) (0.0) (2.0)
        printfn "%s" ("******************")
        printfn "%s" ("Estimating pi using area_under_curve_estimator")
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Estimated value is"); sprintf "%g" (estimated_value)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Expected value is"); sprintf "%g" (PI)|]))
        printfn "%s" (String.concat " " ([|sprintf "%s" ("Total error is"); sprintf "%g" (abs_float (estimated_value - PI))|]))
        printfn "%s" ("******************")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        pi_estimator (1000)
        area_under_line_estimator_check (1000) (0.0) (1.0)
        pi_estimator_using_area_under_curve (1000)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
