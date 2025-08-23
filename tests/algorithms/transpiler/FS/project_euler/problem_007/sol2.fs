// Generated 2025-08-23 14:49 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_prime (number: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if (number > (int64 1)) && (number < (int64 4)) then
            __ret <- true
            raise Return
        else
            if ((number < (int64 2)) || ((((number % (int64 2) + (int64 2)) % (int64 2))) = (int64 0))) || ((((number % (int64 3) + (int64 3)) % (int64 3))) = (int64 0)) then
                __ret <- false
                raise Return
        let mutable i: int64 = int64 5
        while (i * i) <= number do
            if ((((number % i + i) % i)) = (int64 0)) || ((((number % (i + (int64 2)) + (i + (int64 2))) % (i + (int64 2)))) = (int64 0)) then
                __ret <- false
                raise Return
            i <- i + (int64 6)
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution (nth: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable nth = nth
    try
        if nth <= (int64 0) then
            ignore (failwith ("Parameter nth must be greater than or equal to one."))
        let mutable primes: int64 array = Array.empty<int64>
        let mutable num: int64 = int64 2
        while (int64 (Seq.length (primes))) < nth do
            if is_prime (num) then
                primes <- Array.append primes [|num|]
            num <- num + (int64 1)
        __ret <- _idx primes (int ((Seq.length (primes)) - 1))
        raise Return
        __ret
    with
        | Return -> __ret
let ans: int64 = solution (int64 10001)
ignore (printfn "%s" ("solution(10001) = " + (_str (ans))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
