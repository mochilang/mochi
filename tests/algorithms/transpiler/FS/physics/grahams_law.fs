// Generated 2025-08-09 23:14 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec round6 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let factor: float = 1000000.0
        __ret <- (to_float (int ((x * factor) + 0.5))) / factor
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
let rec validate (values: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable values = values
    try
        if (Seq.length (values)) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            if (_idx values (int i)) <= 0.0 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec effusion_ratio (m1: float) (m2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m1 = m1
    let mutable m2 = m2
    try
        if not (validate (unbox<float array> [|m1; m2|])) then
            printfn "%s" ("ValueError: Molar mass values must greater than 0.")
            __ret <- 0.0
            raise Return
        __ret <- round6 (sqrtApprox (m2 / m1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec first_effusion_rate (rate: float) (m1: float) (m2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable rate = rate
    let mutable m1 = m1
    let mutable m2 = m2
    try
        if not (validate (unbox<float array> [|rate; m1; m2|])) then
            printfn "%s" ("ValueError: Molar mass and effusion rate values must greater than 0.")
            __ret <- 0.0
            raise Return
        __ret <- round6 (rate * (sqrtApprox (m2 / m1)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec second_effusion_rate (rate: float) (m1: float) (m2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable rate = rate
    let mutable m1 = m1
    let mutable m2 = m2
    try
        if not (validate (unbox<float array> [|rate; m1; m2|])) then
            printfn "%s" ("ValueError: Molar mass and effusion rate values must greater than 0.")
            __ret <- 0.0
            raise Return
        __ret <- round6 (rate / (sqrtApprox (m2 / m1)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec first_molar_mass (mass: float) (r1: float) (r2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable r1 = r1
    let mutable r2 = r2
    try
        if not (validate (unbox<float array> [|mass; r1; r2|])) then
            printfn "%s" ("ValueError: Molar mass and effusion rate values must greater than 0.")
            __ret <- 0.0
            raise Return
        let ratio: float = r1 / r2
        __ret <- round6 (mass / (ratio * ratio))
        raise Return
        __ret
    with
        | Return -> __ret
let rec second_molar_mass (mass: float) (r1: float) (r2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable r1 = r1
    let mutable r2 = r2
    try
        if not (validate (unbox<float array> [|mass; r1; r2|])) then
            printfn "%s" ("ValueError: Molar mass and effusion rate values must greater than 0.")
            __ret <- 0.0
            raise Return
        let ratio: float = r1 / r2
        __ret <- round6 ((ratio * ratio) / mass)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (effusion_ratio (2.016) (4.002))
printfn "%g" (first_effusion_rate (1.0) (2.016) (4.002))
printfn "%g" (second_effusion_rate (1.0) (2.016) (4.002))
printfn "%g" (first_molar_mass (2.0) (1.408943) (0.709752))
printfn "%g" (second_molar_mass (2.0) (1.408943) (0.709752))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
