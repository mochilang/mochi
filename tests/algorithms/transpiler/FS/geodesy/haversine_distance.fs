// Generated 2025-08-07 16:27 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let AXIS_A: float = 6378137.0
let AXIS_B: float = 6356752.314245
let RADIUS: float = 6378137.0
let rec to_radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec tan_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (sin_taylor (x)) / (cos_taylor (x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
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
let rec atanApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x > 1.0 then
            __ret <- (PI / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        if x < (-1.0) then
            __ret <- ((-PI) / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        __ret <- x / (1.0 + ((0.28 * x) * x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x > 0.0 then
            let ``val``: float = atanApprox (y / x)
            __ret <- ``val``
            raise Return
        if x < 0.0 then
            if y >= 0.0 then
                __ret <- (atanApprox (y / x)) + PI
                raise Return
            __ret <- (atanApprox (y / x)) - PI
            raise Return
        if y > 0.0 then
            __ret <- PI / 2.0
            raise Return
        if y < 0.0 then
            __ret <- (-PI) / 2.0
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec asinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let denom: float = sqrtApprox (1.0 - (x * x))
        let res: float = atan2Approx (x) (denom)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec haversine_distance (lat1: float) (lon1: float) (lat2: float) (lon2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable lat1 = lat1
    let mutable lon1 = lon1
    let mutable lat2 = lat2
    let mutable lon2 = lon2
    try
        let flattening: float = (AXIS_A - AXIS_B) / AXIS_A
        let phi_1: float = atanApprox ((1.0 - flattening) * (tan_approx (to_radians (lat1))))
        let phi_2: float = atanApprox ((1.0 - flattening) * (tan_approx (to_radians (lat2))))
        let lambda_1: float = to_radians (lon1)
        let lambda_2: float = to_radians (lon2)
        let mutable sin_sq_phi: float = sin_taylor ((phi_2 - phi_1) / 2.0)
        let mutable sin_sq_lambda: float = sin_taylor ((lambda_2 - lambda_1) / 2.0)
        sin_sq_phi <- sin_sq_phi * sin_sq_phi
        sin_sq_lambda <- sin_sq_lambda * sin_sq_lambda
        let h_value: float = sqrtApprox (sin_sq_phi + (((cos_taylor (phi_1)) * (cos_taylor (phi_2))) * sin_sq_lambda))
        __ret <- (2.0 * RADIUS) * (asinApprox (h_value))
        raise Return
        __ret
    with
        | Return -> __ret
let SAN_FRANCISCO: float array = [|37.774856; -122.424227|]
let YOSEMITE: float array = [|37.864742; -119.537521|]
printfn "%s" (_str (haversine_distance (_idx SAN_FRANCISCO (0)) (_idx SAN_FRANCISCO (1)) (_idx YOSEMITE (0)) (_idx YOSEMITE (1))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
