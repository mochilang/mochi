// Generated 2025-08-12 13:41 +0700

exception Break
exception Continue

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
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
let rec totients (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    try
        let mutable is_prime: bool array = Array.empty<bool>
        let mutable phi: int array = Array.empty<int>
        let mutable primes: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= limit do
            is_prime <- Array.append is_prime [|true|]
            phi <- Array.append phi [|(i - 1)|]
            i <- i + 1
        i <- 2
        try
            while i <= limit do
                try
                    if _idx is_prime (int i) then
                        primes <- Array.append primes [|i|]
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (primes)) do
                            try
                                let p: int = _idx primes (int j)
                                if (i * p) > limit then
                                    raise Break
                                is_prime.[(i * p)] <- false
                                if (((i % p + p) % p)) = 0 then
                                    phi.[(i * p)] <- (_idx phi (int i)) * p
                                    raise Break
                                phi.[(i * p)] <- (_idx phi (int i)) * (p - 1)
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- phi
        raise Return
        __ret
    with
        | Return -> __ret
and solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable phi: int array = totients (limit)
        let mutable total: int = 0
        let mutable k: int = 2
        while k <= limit do
            total <- total + (_idx phi (int k))
            k <- k + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (1000000))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
