// Generated 2025-08-17 12:28 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec binomial_coefficient (total_elements: int) (elements_to_choose: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable total_elements = total_elements
    let mutable elements_to_choose = elements_to_choose
    try
        if (elements_to_choose = 0) || (elements_to_choose = total_elements) then
            __ret <- 1
            raise Return
        let mutable k: int = elements_to_choose
        if k > (total_elements - k) then
            k <- total_elements - k
        let mutable coefficient: int = 1
        let mutable i: int = 0
        while i < k do
            coefficient <- coefficient * (total_elements - i)
            coefficient <- _floordiv (int coefficient) (int (i + 1))
            i <- i + 1
        __ret <- coefficient
        raise Return
        __ret
    with
        | Return -> __ret
and bell_numbers (max_set_length: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_set_length = max_set_length
    try
        if max_set_length < 0 then
            ignore (failwith ("max_set_length must be non-negative"))
        let mutable bell: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= max_set_length do
            bell <- Array.append bell [|0|]
            i <- i + 1
        bell.[0] <- 1
        i <- 1
        while i <= max_set_length do
            let mutable j: int = 0
            while j < i do
                bell.[i] <- (_idx bell (int i)) + ((binomial_coefficient (i - 1) (j)) * (_idx bell (int j)))
                j <- j + 1
            i <- i + 1
        __ret <- bell
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (_str (bell_numbers (5))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
