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
let rec bubble_sort (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable n: int = Seq.length (arr)
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < ((n - i) - 1) do
                if (_idx arr (int j)) > (_idx arr (int (j + 1))) then
                    let tmp: int = _idx arr (int j)
                    arr.[j] <- _idx arr (int (j + 1))
                    arr.[(j + 1)] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and factors (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        let mutable values: int array = unbox<int array> [|1|]
        let mutable i: int = 2
        while ((int64 i) * (int64 i)) <= (int64 num) do
            if (((num % i + i) % i)) = 0 then
                values <- Array.append values [|i|]
                let d: int = _floordiv (int num) (int i)
                if d <> i then
                    values <- Array.append values [|d|]
            i <- i + 1
        __ret <- bubble_sort (values)
        raise Return
        __ret
    with
        | Return -> __ret
and sum_list (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            total <- total + (_idx xs (int i))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and abundant (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        __ret <- (sum_list (factors (n))) > n
        raise Return
        __ret
    with
        | Return -> __ret
and semi_perfect (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number <= 0 then
            __ret <- true
            raise Return
        let mutable values: int array = factors (number)
        let mutable possible: bool array = Array.empty<bool>
        let mutable j: int = 0
        while j <= number do
            possible <- Array.append possible [|(j = 0)|]
            j <- j + 1
        let mutable idx: int = 0
        while idx < (Seq.length (values)) do
            let v: int = _idx values (int idx)
            let mutable s: int = number
            while s >= v do
                if _idx possible (int (s - v)) then
                    possible.[s] <- true
                s <- s - 1
            idx <- idx + 1
        __ret <- _idx possible (int number)
        raise Return
        __ret
    with
        | Return -> __ret
and weird (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        __ret <- (abundant (number)) && ((semi_perfect (number)) = false)
        raise Return
        __ret
    with
        | Return -> __ret
and run_tests () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (factors (12)) <> [|1; 2; 3; 4; 6|] then
            ignore (failwith ("factors 12 failed"))
        if (factors (1)) <> [|1|] then
            ignore (failwith ("factors 1 failed"))
        if (factors (100)) <> [|1; 2; 4; 5; 10; 20; 25; 50|] then
            ignore (failwith ("factors 100 failed"))
        if (abundant (0)) <> true then
            ignore (failwith ("abundant 0 failed"))
        if (abundant (1)) <> false then
            ignore (failwith ("abundant 1 failed"))
        if (abundant (12)) <> true then
            ignore (failwith ("abundant 12 failed"))
        if (abundant (13)) <> false then
            ignore (failwith ("abundant 13 failed"))
        if (abundant (20)) <> true then
            ignore (failwith ("abundant 20 failed"))
        if (semi_perfect (0)) <> true then
            ignore (failwith ("semi_perfect 0 failed"))
        if (semi_perfect (1)) <> true then
            ignore (failwith ("semi_perfect 1 failed"))
        if (semi_perfect (12)) <> true then
            ignore (failwith ("semi_perfect 12 failed"))
        if (semi_perfect (13)) <> false then
            ignore (failwith ("semi_perfect 13 failed"))
        if (weird (0)) <> false then
            ignore (failwith ("weird 0 failed"))
        if (weird (70)) <> true then
            ignore (failwith ("weird 70 failed"))
        if (weird (77)) <> false then
            ignore (failwith ("weird 77 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (run_tests())
        let nums: int array = unbox<int array> [|69; 70; 71|]
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let n: int = _idx nums (int i)
            if weird (n) then
                ignore (printfn "%s" ((_str (n)) + " is weird."))
            else
                ignore (printfn "%s" ((_str (n)) + " is not weird."))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
