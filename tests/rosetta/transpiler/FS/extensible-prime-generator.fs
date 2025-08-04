// Generated 2025-08-04 20:03 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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
let rec nextPrime (primes: int array) (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable primes = primes
    let mutable start = start
    try
        let mutable n: int = start
        try
            while true do
                try
                    let mutable isP: bool = true
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (primes)) do
                            try
                                let p: int = _idx primes i
                                if (p * p) > n then
                                    raise Break
                                if (((n % p + p) % p)) = 0 then
                                    isP <- false
                                    raise Break
                                i <- i + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if isP then
                        __ret <- n
                        raise Return
                    n <- n + 2
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable primes: int array = [|2|]
        let mutable cand: int = 3
        while (Seq.length (primes)) < 10000 do
            cand <- nextPrime (primes) (cand)
            primes <- Array.append primes [|cand|]
            cand <- cand + 2
        let mutable line: string = "First twenty:"
        let mutable i: int = 0
        while i < 20 do
            line <- (line + " ") + (string (_idx primes i))
            i <- i + 1
        printfn "%s" (line)
        let mutable idx: int = 0
        while (_idx primes idx) <= 100 do
            idx <- idx + 1
        line <- "Between 100 and 150: " + (string (_idx primes idx))
        idx <- idx + 1
        while (_idx primes idx) < 150 do
            line <- (line + " ") + (string (_idx primes idx))
            idx <- idx + 1
        printfn "%s" (line)
        while (_idx primes idx) <= 7700 do
            idx <- idx + 1
        let mutable count: int = 0
        while (_idx primes idx) < 8000 do
            count <- count + 1
            idx <- idx + 1
        printfn "%s" ("Number beween 7,700 and 8,000: " + (string (count)))
        printfn "%s" ("10,000th prime: " + (string (_idx primes 9999)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
