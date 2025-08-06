// Generated 2025-08-06 23:33 +0700

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
let rec base16_encode (data: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data = data
    try
        let digits: string = "0123456789ABCDEF"
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            let b: int = _idx data (i)
            if (b < 0) || (b > 255) then
                failwith ("byte out of range")
            let hi: int = b / 16
            let lo: int = ((b % 16 + 16) % 16)
            res <- (res + (digits.Substring(hi, (hi + 1) - hi))) + (digits.Substring(lo, (lo + 1) - lo))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and base16_decode (data: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    try
        let digits: string = "0123456789ABCDEF"
        if ((((String.length (data)) % 2 + 2) % 2)) <> 0 then
            failwith ("Base16 encoded data is invalid: Data does not have an even number of hex digits.")
        let rec hex_value (ch: string) =
            let mutable __ret : int = Unchecked.defaultof<int>
            let mutable ch = ch
            try
                let mutable j: int = 0
                while j < 16 do
                    if (digits.Substring(j, (j + 1) - j)) = ch then
                        __ret <- j
                        raise Return
                    j <- j + 1
                __ret <- -1
                raise Return
                __ret
            with
                | Return -> __ret
        let mutable out: int array = [||]
        let mutable i: int = 0
        while i < (String.length (data)) do
            let hi_char: string = data.Substring(i, (i + 1) - i)
            let lo_char: string = data.Substring(i + 1, (i + 2) - (i + 1))
            let hi: int = hex_value (hi_char)
            let lo: int = hex_value (lo_char)
            if (hi < 0) || (lo < 0) then
                failwith ("Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters.")
            out <- Array.append out [|(hi * 16) + lo|]
            i <- i + 2
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let example1: int array = [|72; 101; 108; 108; 111; 32; 87; 111; 114; 108; 100; 33|]
let example2: int array = [|72; 69; 76; 76; 79; 32; 87; 79; 82; 76; 68; 33|]
printfn "%s" (base16_encode (example1))
printfn "%s" (base16_encode (example2))
printfn "%s" (base16_encode (Array.empty<int>))
printfn "%s" (_str (base16_decode ("48656C6C6F20576F726C6421")))
printfn "%s" (_str (base16_decode ("48454C4C4F20574F524C4421")))
printfn "%s" (_str (base16_decode ("")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
