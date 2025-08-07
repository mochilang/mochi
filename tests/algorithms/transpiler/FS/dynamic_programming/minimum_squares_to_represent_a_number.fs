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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_list (len: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable len = len
    let mutable value = value
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < len do
            arr <- Array.append arr [|value|]
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        while ((r + 1) * (r + 1)) <= n do
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec minimum_squares_to_represent_a_number (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        if number < 0 then
            failwith ("the value of input must not be a negative number")
        if number = 0 then
            __ret <- 1
            raise Return
        let mutable answers: int array = make_list (number + 1) (-1)
        answers.[0] <- 0
        let mutable i: int = 1
        while i <= number do
            let mutable answer: int = i
            let root: int = int_sqrt (i)
            let mutable j: int = 1
            while j <= root do
                let current_answer: int = 1 + (_idx answers (i - (j * j)))
                if current_answer < answer then
                    answer <- current_answer
                j <- j + 1
            answers.[i] <- answer
            i <- i + 1
        __ret <- _idx answers (number)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (minimum_squares_to_represent_a_number (25))
printfn "%d" (minimum_squares_to_represent_a_number (21))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
