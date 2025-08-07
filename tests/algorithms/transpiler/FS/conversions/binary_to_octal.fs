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
let rec bin_to_octal (bin_string: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bin_string = bin_string
    try
        let mutable i: int = 0
        while i < (String.length (bin_string)) do
            let c: string = string (bin_string.[i])
            if not ((c = "0") || (c = "1")) then
                failwith ("Non-binary value was passed to the function")
            i <- i + 1
        if (String.length (bin_string)) = 0 then
            failwith ("Empty string was passed to the function")
        let mutable padded: string = bin_string
        while ((((String.length (padded)) % 3 + 3) % 3)) <> 0 do
            padded <- "0" + padded
        let mutable oct_string: string = ""
        let mutable index: int = 0
        while index < (String.length (padded)) do
            let group: string = padded.Substring(index, (index + 3) - index)
            let b0: int = if (string (group.[0])) = "1" then 1 else 0
            let b1: int = if (string (group.[1])) = "1" then 1 else 0
            let b2: int = if (string (group.[2])) = "1" then 1 else 0
            let oct_val: int = ((b0 * 4) + (b1 * 2)) + b2
            oct_string <- oct_string + (_str (oct_val))
            index <- index + 3
        __ret <- oct_string
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (bin_to_octal ("1111"))
printfn "%s" (bin_to_octal ("101010101010011"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
