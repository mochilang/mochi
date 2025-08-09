// Generated 2025-08-09 16:21 +0700

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
let rec pow (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt_approx (x: float) =
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
and hubble_parameter (hubble_constant: float) (radiation_density: float) (matter_density: float) (dark_energy: float) (redshift: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable hubble_constant = hubble_constant
    let mutable radiation_density = radiation_density
    let mutable matter_density = matter_density
    let mutable dark_energy = dark_energy
    let mutable redshift = redshift
    try
        let parameters: float array = unbox<float array> [|redshift; radiation_density; matter_density; dark_energy|]
        let mutable i: int = 0
        while i < (Seq.length (parameters)) do
            if (_idx parameters (int i)) < 0.0 then
                failwith ("All input parameters must be positive")
            i <- i + 1
        i <- 1
        while i < 4 do
            if (_idx parameters (int i)) > 1.0 then
                failwith ("Relative densities cannot be greater than one")
            i <- i + 1
        let curvature: float = 1.0 - ((matter_density + radiation_density) + dark_energy)
        let zp1: float = redshift + 1.0
        let e2: float = (((radiation_density * (pow (zp1) (4))) + (matter_density * (pow (zp1) (3)))) + (curvature * (pow (zp1) (2)))) + dark_energy
        __ret <- hubble_constant * (sqrt_approx (e2))
        raise Return
        __ret
    with
        | Return -> __ret
and test_hubble_parameter () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let h: float = hubble_parameter (68.3) (0.0001) (0.3) (0.7) (0.0)
        if (h < 68.2999) || (h > 68.3001) then
            failwith ("hubble_parameter test failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_hubble_parameter()
        printfn "%g" (hubble_parameter (68.3) (0.0001) (0.3) (0.7) (0.0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
