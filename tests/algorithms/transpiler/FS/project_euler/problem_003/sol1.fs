// Generated 2025-08-23 14:49 +0700

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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec is_prime (number: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if (number > (int64 1)) && (number < (int64 4)) then
            __ret <- true
            raise Return
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
and solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable num: int64 = n
        if num <= (int64 0) then
            ignore (printfn "%s" ("Parameter n must be greater than or equal to one."))
            __ret <- int64 0
            raise Return
        if is_prime (num) then
            __ret <- num
            raise Return
        while (((num % (int64 2) + (int64 2)) % (int64 2))) = (int64 0) do
            num <- _floordiv64 (int64 num) (int64 (int64 2))
            if is_prime (num) then
                __ret <- num
                raise Return
        let mutable max_number: int64 = int64 1
        let mutable i: int64 = int64 3
        try
            while (i * i) <= num do
                try
                    if (((num % i + i) % i)) = (int64 0) then
                        if is_prime (_floordiv64 (int64 num) (int64 i)) then
                            max_number <- _floordiv64 (int64 num) (int64 i)
                            raise Break
                        else
                            if is_prime (i) then
                                max_number <- i
                    i <- i + (int64 2)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- max_number
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result: int64 = solution (600851475143L)
        ignore (printfn "%s" ("solution() = " + (_str (result))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
