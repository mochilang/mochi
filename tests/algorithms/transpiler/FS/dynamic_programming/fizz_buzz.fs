// Generated 2025-08-07 15:46 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec fizz_buzz (number: int) (iterations: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable iterations = iterations
    try
        if number < 1 then
            failwith ("starting number must be an integer and be more than 0")
        if iterations < 1 then
            failwith ("Iterations must be done more than 0 times to play FizzBuzz")
        let mutable out: string = ""
        let mutable n: int = number
        while n <= iterations do
            if (((n % 3 + 3) % 3)) = 0 then
                out <- out + "Fizz"
            if (((n % 5 + 5) % 5)) = 0 then
                out <- out + "Buzz"
            if ((((n % 3 + 3) % 3)) <> 0) && ((((n % 5 + 5) % 5)) <> 0) then
                out <- out + (_str (n))
            out <- out + " "
            n <- n + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (fizz_buzz (1) (7))
printfn "%s" (fizz_buzz (1) (15))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
