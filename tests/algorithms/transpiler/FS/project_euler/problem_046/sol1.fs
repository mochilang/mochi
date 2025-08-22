// Generated 2025-08-22 15:25 +0700

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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        while ((int64 (r + 1)) * (int64 (r + 1))) <= (int64 n) do
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and is_prime (number: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if (1 < number) && (number < 4) then
            __ret <- true
            raise Return
        if ((number < 2) || ((((number % 2 + 2) % 2)) = 0)) || ((((number % 3 + 3) % 3)) = 0) then
            __ret <- false
            raise Return
        let mutable i: int = 5
        let limit: int = int_sqrt (number)
        while i <= limit do
            if ((((number % i + i) % i)) = 0) || ((((number % (i + 2) + (i + 2)) % (i + 2))) = 0) then
                __ret <- false
                raise Return
            i <- i + 6
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and compute_nums (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n <= 0 then
            ignore (failwith ("n must be >= 0"))
        let mutable list_nums: int array = Array.empty<int>
        let mutable num: int = 3
        try
            while (Seq.length (list_nums)) < n do
                try
                    if not (is_prime (num)) then
                        let mutable i: int = 0
                        let mutable found: bool = false
                        try
                            while (((int64 2) * (int64 i)) * (int64 i)) <= (int64 num) do
                                try
                                    let rem: int64 = (int64 num) - (((int64 2) * (int64 i)) * (int64 i))
                                    if is_prime (rem) then
                                        found <- true
                                        raise Break
                                    i <- i + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        if not found then
                            list_nums <- Array.append list_nums [|num|]
                    num <- num + 2
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- list_nums
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        __ret <- _idx (compute_nums (1)) (int 0)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution())))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
