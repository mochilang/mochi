// Generated 2025-08-12 07:47 +0700

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
let rec split_by_dot (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if c = "." then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + c
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit_str (s: string) =
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
and parse_decimal (s: string) =
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
and is_ip_v4_address_valid (ip: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ip = ip
    try
        let octets: string array = split_by_dot (ip)
        if (Seq.length (octets)) <> 4 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < 4 do
            let oct: string = _idx octets (int i)
            if not (is_digit_str (oct)) then
                __ret <- false
                raise Return
            let number: int = parse_decimal (oct)
            if (String.length (_str (number))) <> (String.length (oct)) then
                __ret <- false
                raise Return
            if (number < 0) || (number > 255) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_ip_v4_address_valid ("192.168.0.23")))
printfn "%s" (_str (is_ip_v4_address_valid ("192.256.15.8")))
printfn "%s" (_str (is_ip_v4_address_valid ("172.100.0.8")))
printfn "%s" (_str (is_ip_v4_address_valid ("255.256.0.256")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.33333333.4")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.-3.4")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.3")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.3.4.5")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.A.4")))
printfn "%s" (_str (is_ip_v4_address_valid ("0.0.0.0")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.3.")))
printfn "%s" (_str (is_ip_v4_address_valid ("1.2.3.05")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
