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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec rstrip_s (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- if ((String.length (s)) > 0) && ((string (s.[(String.length (s)) - 1])) = "s") then (_substring s 0 ((String.length (s)) - 1)) else s
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalize_alias (u: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable u = u
    try
        if u = "millimeter" then
            __ret <- "mm"
            raise Return
        if u = "centimeter" then
            __ret <- "cm"
            raise Return
        if u = "meter" then
            __ret <- "m"
            raise Return
        if u = "kilometer" then
            __ret <- "km"
            raise Return
        if u = "inch" then
            __ret <- "in"
            raise Return
        if u = "inche" then
            __ret <- "in"
            raise Return
        if u = "feet" then
            __ret <- "ft"
            raise Return
        if u = "foot" then
            __ret <- "ft"
            raise Return
        if u = "yard" then
            __ret <- "yd"
            raise Return
        if u = "mile" then
            __ret <- "mi"
            raise Return
        __ret <- u
        raise Return
        __ret
    with
        | Return -> __ret
let rec has_unit (u: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable u = u
    try
        __ret <- (((((((u = "mm") || (u = "cm")) || (u = "m")) || (u = "km")) || (u = "in")) || (u = "ft")) || (u = "yd")) || (u = "mi")
        raise Return
        __ret
    with
        | Return -> __ret
let rec from_factor (u: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable u = u
    try
        if u = "mm" then
            __ret <- 0.001
            raise Return
        if u = "cm" then
            __ret <- 0.01
            raise Return
        if u = "m" then
            __ret <- 1.0
            raise Return
        if u = "km" then
            __ret <- 1000.0
            raise Return
        if u = "in" then
            __ret <- 0.0254
            raise Return
        if u = "ft" then
            __ret <- 0.3048
            raise Return
        if u = "yd" then
            __ret <- 0.9144
            raise Return
        if u = "mi" then
            __ret <- 1609.34
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_factor (u: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable u = u
    try
        if u = "mm" then
            __ret <- 1000.0
            raise Return
        if u = "cm" then
            __ret <- 100.0
            raise Return
        if u = "m" then
            __ret <- 1.0
            raise Return
        if u = "km" then
            __ret <- 0.001
            raise Return
        if u = "in" then
            __ret <- 39.3701
            raise Return
        if u = "ft" then
            __ret <- 3.28084
            raise Return
        if u = "yd" then
            __ret <- 1.09361
            raise Return
        if u = "mi" then
            __ret <- 0.000621371
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec length_conversion (value: float) (from_type: string) (to_type: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable from_type = from_type
    let mutable to_type = to_type
    try
        let mutable new_from: string = normalize_alias (rstrip_s (unbox<string> (from_type.ToLower())))
        let mutable new_to: string = normalize_alias (rstrip_s (unbox<string> (to_type.ToLower())))
        if not (has_unit (new_from)) then
            failwith (("Invalid 'from_type' value: '" + from_type) + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")
        if not (has_unit (new_to)) then
            failwith (("Invalid 'to_type' value: '" + to_type) + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")
        __ret <- (value * (from_factor (new_from))) * (to_factor (new_to))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (length_conversion (4.0) ("METER") ("FEET"))
printfn "%g" (length_conversion (1.0) ("kilometer") ("inch"))
printfn "%g" (length_conversion (2.0) ("feet") ("meter"))
printfn "%g" (length_conversion (2.0) ("centimeter") ("millimeter"))
printfn "%g" (length_conversion (4.0) ("yard") ("kilometer"))
printfn "%g" (length_conversion (3.0) ("foot") ("inch"))
printfn "%g" (length_conversion (3.0) ("mm") ("in"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
