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
let hex_digits: string = "0123456789abcdef"
let rec split_by_dot (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = [||]
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
let rec parse_decimal (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        if (String.length (s)) = 0 then
            failwith ("Invalid IPv4 address format")
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c < "0") || (c > "9") then
                failwith ("Invalid IPv4 address format")
            value <- (value * 10) + (int c)
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_hex2 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable x: int = n
        let mutable res: string = ""
        while x > 0 do
            let d: int = ((x % 16 + 16) % 16)
            res <- (string (hex_digits.[d])) + res
            x <- x / 16
        while (String.length (res)) < 2 do
            res <- "0" + res
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec ipv4_to_decimal (ipv4_address: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ipv4_address = ipv4_address
    try
        let mutable parts: string array = split_by_dot (ipv4_address)
        if (Seq.length (parts)) <> 4 then
            failwith ("Invalid IPv4 address format")
        let mutable result: int = 0
        let mutable i: int = 0
        while i < 4 do
            let oct: int = parse_decimal (_idx parts (i))
            if (oct < 0) || (oct > 255) then
                failwith ("Invalid IPv4 octet " + (_str (oct)))
            result <- (result * 256) + oct
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec alt_ipv4_to_decimal (ipv4_address: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ipv4_address = ipv4_address
    try
        let mutable parts: string array = split_by_dot (ipv4_address)
        if (Seq.length (parts)) <> 4 then
            failwith ("Invalid IPv4 address format")
        let mutable hex_str: string = ""
        let mutable i: int = 0
        while i < 4 do
            let oct: int = parse_decimal (_idx parts (i))
            if (oct < 0) || (oct > 255) then
                failwith ("Invalid IPv4 octet " + (_str (oct)))
            hex_str <- hex_str + (to_hex2 (oct))
            i <- i + 1
        let mutable value: int = 0
        let mutable k: int = 0
        while k < (String.length (hex_str)) do
            let c: string = string (hex_str.[k])
            let mutable digit: int = 0 - 1
            let mutable j: int = 0
            while j < (String.length (hex_digits)) do
                if (string (hex_digits.[j])) = c then
                    digit <- j
                j <- j + 1
            if digit < 0 then
                failwith ("Invalid hex digit")
            value <- (value * 16) + digit
            k <- k + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec decimal_to_ipv4 (decimal_ipv4: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable decimal_ipv4 = decimal_ipv4
    try
        if (decimal_ipv4 < 0) || ((int64 decimal_ipv4) > 4294967295L) then
            failwith ("Invalid decimal IPv4 address")
        let mutable n: int = decimal_ipv4
        let mutable parts: string array = [||]
        let mutable i: int = 0
        while i < 4 do
            let octet: int = ((n % 256 + 256) % 256)
            parts <- Array.append parts [|_str (octet)|]
            n <- n / 256
            i <- i + 1
        let mutable res: string = ""
        let mutable j: int = (Seq.length (parts)) - 1
        while j >= 0 do
            res <- res + (_idx parts (j))
            if j > 0 then
                res <- res + "."
            j <- j - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (ipv4_to_decimal ("192.168.0.1"))
printfn "%d" (ipv4_to_decimal ("10.0.0.255"))
printfn "%d" (alt_ipv4_to_decimal ("192.168.0.1"))
printfn "%d" (alt_ipv4_to_decimal ("10.0.0.255"))
printfn "%s" (decimal_to_ipv4 (int 3232235521L))
printfn "%s" (decimal_to_ipv4 (167772415))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
