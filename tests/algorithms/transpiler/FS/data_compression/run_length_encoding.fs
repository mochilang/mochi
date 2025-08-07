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
let rec run_length_encode (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        if (String.length (text)) = 0 then
            __ret <- ""
            raise Return
        let mutable encoded: string = ""
        let mutable count: int = 1
        let mutable i: int = 0
        while i < (String.length (text)) do
            if ((i + 1) < (String.length (text))) && ((string (text.[i])) = (string (text.[i + 1]))) then
                count <- count + 1
            else
                encoded <- (encoded + (string (text.[i]))) + (_str (count))
                count <- 1
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_length_decode (encoded: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable encoded = encoded
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (encoded)) do
            let ch: string = string (encoded.[i])
            i <- i + 1
            let mutable num_str: string = ""
            while ((i < (String.length (encoded))) && ((string (encoded.[i])) >= "0")) && ((string (encoded.[i])) <= "9") do
                num_str <- num_str + (string (encoded.[i]))
                i <- i + 1
            let mutable count: int = int (num_str)
            let mutable j: int = 0
            while j < count do
                res <- res + ch
                j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let example1: string = "AAAABBBCCDAA"
let encoded1: string = run_length_encode (example1)
printfn "%s" (encoded1)
printfn "%s" (run_length_decode (encoded1))
let example2: string = "A"
let encoded2: string = run_length_encode (example2)
printfn "%s" (encoded2)
printfn "%s" (run_length_decode (encoded2))
let example3: string = "AAADDDDDDFFFCCCAAVVVV"
let encoded3: string = run_length_encode (example3)
printfn "%s" (encoded3)
printfn "%s" (run_length_decode (encoded3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
