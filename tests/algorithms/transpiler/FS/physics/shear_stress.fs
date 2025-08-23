// Generated 2025-08-23 14:49 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
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
let rec shear_stress (stress: float) (tangential_force: float) (area: float) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable stress = stress
    let mutable tangential_force = tangential_force
    let mutable area = area
    try
        let mutable zeros: int = 0
        if abs(stress - 0.0) < 1e-9 then
            zeros <- zeros + 1
        if abs(tangential_force - 0.0) < 1e-9 then
            zeros <- zeros + 1
        if abs(area - 0.0) < 1e-9 then
            zeros <- zeros + 1
        if zeros <> 1 then
            ignore (failwith ("You cannot supply more or less than 2 values"))
        else
            if stress < 0.0 then
                ignore (failwith ("Stress cannot be negative"))
            else
                if tangential_force < 0.0 then
                    ignore (failwith ("Tangential Force cannot be negative"))
                else
                    if area < 0.0 then
                        ignore (failwith ("Area cannot be negative"))
                    else
                        if abs(stress - 0.0) < 1e-9 then
                            __ret <- { _name = "stress"; _value = tangential_force / area }
                            raise Return
                        else
                            if abs(tangential_force - 0.0) < 1e-9 then
                                __ret <- { _name = "tangential_force"; _value = stress * area }
                                raise Return
                            else
                                __ret <- { _name = "area"; _value = tangential_force / stress }
                                raise Return
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
let r1: Result = shear_stress (25.0) (100.0) (0.0)
ignore (printfn "%s" (str_result (r1)))
let r2: Result = shear_stress (0.0) (1600.0) (200.0)
ignore (printfn "%s" (str_result (r2)))
let r3: Result = shear_stress (1000.0) (0.0) (1200.0)
ignore (printfn "%s" (str_result (r3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
