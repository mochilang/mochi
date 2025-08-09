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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        if n >= 0 then
            let mutable i: int = 0
            while i < n do
                p <- p * 10.0
                i <- i + 1
        else
            let mutable i: int = 0
            while i > n do
                p <- p / 10.0
                i <- i - 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        let f: float = float i
        if f > x then
            __ret <- float (i - 1)
            raise Return
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and format_scientific_3 (x: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    try
        if x = 0.0 then
            __ret <- "0.000e+00"
            raise Return
        let mutable sign: string = ""
        let mutable num: float = x
        if num < 0.0 then
            sign <- "-"
            num <- -num
        let mutable exp: int = 0
        while num >= 10.0 do
            num <- num / 10.0
            exp <- exp + 1
        while num < 1.0 do
            num <- num * 10.0
            exp <- exp - 1
        let temp: float = floor ((num * 1000.0) + 0.5)
        let mutable scaled: int = int temp
        if scaled = 10000 then
            scaled <- 1000
            exp <- exp + 1
        let int_part: int = _floordiv scaled 1000
        let mutable frac_part: int = ((scaled % 1000 + 1000) % 1000)
        let mutable frac_str: string = _str (frac_part)
        while (String.length (frac_str)) < 3 do
            frac_str <- "0" + frac_str
        let mantissa: string = ((_str (int_part)) + ".") + frac_str
        let mutable exp_sign: string = "+"
        let mutable exp_abs: int = exp
        if exp < 0 then
            exp_sign <- "-"
            exp_abs <- -exp
        let mutable exp_str: string = _str (exp_abs)
        if exp_abs < 10 then
            exp_str <- "0" + exp_str
        __ret <- (((sign + mantissa) + "e") + exp_sign) + exp_str
        raise Return
        __ret
    with
        | Return -> __ret
and orbital_transfer_work (mass_central: float) (mass_object: float) (r_initial: float) (r_final: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable mass_central = mass_central
    let mutable mass_object = mass_object
    let mutable r_initial = r_initial
    let mutable r_final = r_final
    try
        let G: float = 6.6743 * (pow10 (-11))
        if (r_initial <= 0.0) || (r_final <= 0.0) then
            failwith ("Orbital radii must be greater than zero.")
        let work: float = (((G * mass_central) * mass_object) / 2.0) * ((1.0 / r_initial) - (1.0 / r_final))
        __ret <- format_scientific_3 (work)
        raise Return
        __ret
    with
        | Return -> __ret
and test_orbital_transfer_work () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (orbital_transfer_work (5.972 * (pow10 (24))) (1000.0) (6.371 * (pow10 (6))) (7.0 * (pow10 (6)))) <> "2.811e+09" then
            failwith ("case1 failed")
        if (orbital_transfer_work (5.972 * (pow10 (24))) (500.0) (7.0 * (pow10 (6))) (6.371 * (pow10 (6)))) <> "-1.405e+09" then
            failwith ("case2 failed")
        if (orbital_transfer_work (1.989 * (pow10 (30))) (1000.0) (1.5 * (pow10 (11))) (2.28 * (pow10 (11)))) <> "1.514e+11" then
            failwith ("case3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_orbital_transfer_work()
        printfn "%s" (orbital_transfer_work (5.972 * (pow10 (24))) (1000.0) (6.371 * (pow10 (6))) (7.0 * (pow10 (6))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
