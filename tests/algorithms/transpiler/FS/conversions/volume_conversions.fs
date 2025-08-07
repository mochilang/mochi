// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let units: string array = [|"cubic meter"; "litre"; "kilolitre"; "gallon"; "cubic yard"; "cubic foot"; "cup"|]
let from_factors: float array = [|1.0; 0.001; 1.0; 0.00454; 0.76455; 0.028; 0.000236588|]
let to_factors: float array = [|1.0; 1000.0; 1.0; 264.172; 1.30795; 35.3147; 4226.75|]
let rec supported_values () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let mutable result: string = _idx units (0)
        let mutable i: int = 1
        while i < (Seq.length (units)) do
            result <- (result + ", ") + (_idx units (i))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_index (name: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable name = name
    try
        let mutable i: int = 0
        while i < (Seq.length (units)) do
            if (_idx units (i)) = name then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_from_factor (name: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable name = name
    try
        let idx: int = find_index (name)
        if idx < 0 then
            failwith ((("Invalid 'from_type' value: '" + name) + "' Supported values are: ") + (supported_values()))
        __ret <- _idx from_factors (idx)
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_to_factor (name: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable name = name
    try
        let idx: int = find_index (name)
        if idx < 0 then
            failwith ((("Invalid 'to_type' value: '" + name) + "' Supported values are: ") + (supported_values()))
        __ret <- _idx to_factors (idx)
        raise Return
        __ret
    with
        | Return -> __ret
let rec volume_conversion (value: float) (from_type: string) (to_type: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable from_type = from_type
    let mutable to_type = to_type
    try
        let from_factor: float = get_from_factor (from_type)
        let to_factor: float = get_to_factor (to_type)
        __ret <- (value * from_factor) * to_factor
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (volume_conversion (4.0) ("cubic meter") ("litre")))
printfn "%s" (_str (volume_conversion (1.0) ("litre") ("gallon")))
printfn "%s" (_str (volume_conversion (1.0) ("kilolitre") ("cubic meter")))
printfn "%s" (_str (volume_conversion (3.0) ("gallon") ("cubic yard")))
printfn "%s" (_str (volume_conversion (2.0) ("cubic yard") ("litre")))
printfn "%s" (_str (volume_conversion (4.0) ("cubic foot") ("cup")))
printfn "%s" (_str (volume_conversion (1.0) ("cup") ("kilolitre")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
