// Generated 2025-08-13 16:13 +0700

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
type Result = {
    mutable _name: string
    mutable _value: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and round_to (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (float (floor ((x * m) + 0.5))) / m
        raise Return
        __ret
    with
        | Return -> __ret
and electric_power (voltage: float) (current: float) (power: float) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable voltage = voltage
    let mutable current = current
    let mutable power = power
    try
        let mutable zeros: int = 0
        if voltage = 0.0 then
            zeros <- zeros + 1
        if current = 0.0 then
            zeros <- zeros + 1
        if power = 0.0 then
            zeros <- zeros + 1
        if zeros <> 1 then
            ignore (failwith ("Exactly one argument must be 0"))
        else
            if power < 0.0 then
                ignore (failwith ("Power cannot be negative in any electrical/electronics system"))
            else
                if voltage = 0.0 then
                    __ret <- { _name = "voltage"; _value = power / current }
                    raise Return
                else
                    if current = 0.0 then
                        __ret <- { _name = "current"; _value = power / voltage }
                        raise Return
                    else
                        if power = 0.0 then
                            let mutable p: float = absf (voltage * current)
                            __ret <- { _name = "power"; _value = round_to (p) (2) }
                            raise Return
                        else
                            ignore (failwith ("Unhandled case"))
        __ret
    with
        | Return -> __ret
and str_result (r: Result) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    try
        __ret <- ((("Result(name='" + (r._name)) + "', value=") + (_str (r._value))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Result = electric_power (0.0) (2.0) (5.0)
ignore (printfn "%s" (str_result (r1)))
let r2: Result = electric_power (2.0) (2.0) (0.0)
ignore (printfn "%s" (str_result (r2)))
let r3: Result = electric_power (-2.0) (3.0) (0.0)
ignore (printfn "%s" (str_result (r3)))
let r4: Result = electric_power (2.2) (2.2) (0.0)
ignore (printfn "%s" (str_result (r4)))
let r5: Result = electric_power (2.0) (0.0) (6.0)
ignore (printfn "%s" (str_result (r5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
