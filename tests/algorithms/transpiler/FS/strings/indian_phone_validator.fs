// Generated 2025-08-11 17:23 +0700

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
let rec all_digits (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if (String.length (s)) = 0 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c < "0") || (c > "9") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and indian_phone_validator (phone: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable phone = phone
    try
        let mutable s: string = phone
        if ((String.length (s)) >= 3) && ((_substring s 0 3) = "+91") then
            s <- _substring s 3 (String.length (s))
            if (String.length (s)) > 0 then
                let c: string = string (s.[0])
                if (c = "-") || (c = " ") then
                    s <- _substring s 1 (String.length (s))
        if ((String.length (s)) > 0) && ((string (s.[0])) = "0") then
            s <- _substring s 1 (String.length (s))
        if ((String.length (s)) >= 2) && ((_substring s 0 2) = "91") then
            s <- _substring s 2 (String.length (s))
        if (String.length (s)) <> 10 then
            __ret <- false
            raise Return
        let first: string = string (s.[0])
        if not (((first = "7") || (first = "8")) || (first = "9")) then
            __ret <- false
            raise Return
        if not (all_digits (s)) then
            __ret <- false
            raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (indian_phone_validator ("+91123456789")))
printfn "%s" (_str (indian_phone_validator ("+919876543210")))
printfn "%s" (_str (indian_phone_validator ("01234567896")))
printfn "%s" (_str (indian_phone_validator ("919876543218")))
printfn "%s" (_str (indian_phone_validator ("+91-1234567899")))
printfn "%s" (_str (indian_phone_validator ("+91-9876543218")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
