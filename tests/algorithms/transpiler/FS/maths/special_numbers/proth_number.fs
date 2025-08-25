// Generated 2025-08-25 22:27 +0700

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
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec pow2 (exp: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable exp = exp
    try
        let mutable result: int64 = int64 1
        let mutable i: int64 = int64 0
        while i < exp do
            result <- result * (int64 2)
            i <- i + (int64 1)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and proth (number: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable number = number
    try
        if number < (int64 1) then
            ignore (failwith ("Input value must be > 0"))
        if number = (int64 1) then
            __ret <- int64 3
            raise Return
        if number = (int64 2) then
            __ret <- int64 5
            raise Return
        let temp: int64 = int64 (_floordiv64 (int64 number) (int64 (int64 3)))
        let mutable pow: int64 = int64 1
        let mutable block_index: int64 = int64 1
        while pow <= temp do
            pow <- pow * (int64 2)
            block_index <- block_index + (int64 1)
        let mutable proth_list: int64 array = Array.map int64 [|3; 5|]
        let mutable proth_index: int64 = int64 2
        let mutable increment: int64 = int64 3
        let mutable block: int64 = int64 1
        while block < block_index do
            let mutable i: int64 = int64 0
            while i < increment do
                let next_val: int64 = (pow2 (int64 (block + (int64 1)))) + (_idx proth_list (int (proth_index - (int64 1))))
                proth_list <- Array.append proth_list [|next_val|]
                proth_index <- proth_index + (int64 1)
                i <- i + (int64 1)
            increment <- increment * (int64 2)
            block <- block + (int64 1)
        __ret <- _idx proth_list (int (number - (int64 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable n: int64 = int64 1
        while n <= (int64 10) do
            let value: int64 = proth (int64 n)
            ignore (printfn "%s" ((("The " + (_str (n))) + "th Proth number: ") + (_str (value))))
            n <- n + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
