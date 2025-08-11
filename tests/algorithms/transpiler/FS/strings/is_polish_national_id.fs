// Generated 2025-08-11 15:32 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec parse_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            value <- int (((int64 value) * (int64 10)) + (int64 (int c)))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_polish_national_id (id: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable id = id
    try
        if (String.length (id)) = 0 then
            __ret <- false
            raise Return
        if (_substring id 0 1) = "-" then
            __ret <- false
            raise Return
        let input_int: int = parse_int (id)
        if (input_int < 10100000) || ((int64 input_int) > 99923199999L) then
            __ret <- false
            raise Return
        let month: int = parse_int (_substring id 2 4)
        if not ((((((month >= 1) && (month <= 12)) || ((month >= 21) && (month <= 32))) || ((month >= 41) && (month <= 52))) || ((month >= 61) && (month <= 72))) || ((month >= 81) && (month <= 92))) then
            __ret <- false
            raise Return
        let day: int = parse_int (_substring id 4 6)
        if (day < 1) || (day > 31) then
            __ret <- false
            raise Return
        let multipliers: int array = unbox<int array> [|1; 3; 7; 9; 1; 3; 7; 9; 1; 3|]
        let mutable subtotal: int = 0
        let mutable i: int = 0
        while i < (Seq.length (multipliers)) do
            let digit: int = parse_int (_substring id i (i + 1))
            subtotal <- int ((int64 subtotal) + (((((int64 digit) * (int64 (_idx multipliers (int i)))) % (int64 10) + (int64 10)) % (int64 10))))
            i <- i + 1
        let checksum: int = 10 - (((subtotal % 10 + 10) % 10))
        __ret <- checksum = (((input_int % 10 + 10) % 10))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_polish_national_id ("02070803628")))
printfn "%s" (_str (is_polish_national_id ("02150803629")))
printfn "%s" (_str (is_polish_national_id ("02075503622")))
printfn "%s" (_str (is_polish_national_id ("-99012212349")))
printfn "%s" (_str (is_polish_national_id ("990122123499999")))
printfn "%s" (_str (is_polish_national_id ("02070803621")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
