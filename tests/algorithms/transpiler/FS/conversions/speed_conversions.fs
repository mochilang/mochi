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
let units: string array = [|"km/h"; "m/s"; "mph"; "knot"|]
let speed_chart: float array = [|1.0; 3.6; 1.609344; 1.852|]
let speed_chart_inverse: float array = [|1.0; 0.277777778; 0.621371192; 0.539956803|]
let rec index_of (arr: string array) (value: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) = value then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec units_string (arr: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable arr = arr
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if i > 0 then
                s <- s + ", "
            s <- s + (_idx arr (i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec round3 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (x * 1000.0) + 0.5
        let z: int = int y
        let zf: float = float z
        __ret <- zf / 1000.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_speed (speed: float) (unit_from: string) (unit_to: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable speed = speed
    let mutable unit_from = unit_from
    let mutable unit_to = unit_to
    try
        let from_index: int = index_of (units) (unit_from)
        let to_index: int = index_of (units) (unit_to)
        if (from_index < 0) || (to_index < 0) then
            let msg: string = (((("Incorrect 'from_type' or 'to_type' value: " + unit_from) + ", ") + unit_to) + "\nValid values are: ") + (units_string (units))
            failwith (msg)
        let result: float = (speed * (_idx speed_chart (from_index))) * (_idx speed_chart_inverse (to_index))
        let r: float = round3 (result)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (convert_speed (100.0) ("km/h") ("m/s")))
printfn "%s" (_str (convert_speed (100.0) ("km/h") ("mph")))
printfn "%s" (_str (convert_speed (100.0) ("km/h") ("knot")))
printfn "%s" (_str (convert_speed (100.0) ("m/s") ("km/h")))
printfn "%s" (_str (convert_speed (100.0) ("m/s") ("mph")))
printfn "%s" (_str (convert_speed (100.0) ("m/s") ("knot")))
printfn "%s" (_str (convert_speed (100.0) ("mph") ("km/h")))
printfn "%s" (_str (convert_speed (100.0) ("mph") ("m/s")))
printfn "%s" (_str (convert_speed (100.0) ("mph") ("knot")))
printfn "%s" (_str (convert_speed (100.0) ("knot") ("km/h")))
printfn "%s" (_str (convert_speed (100.0) ("knot") ("m/s")))
printfn "%s" (_str (convert_speed (100.0) ("knot") ("mph")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
