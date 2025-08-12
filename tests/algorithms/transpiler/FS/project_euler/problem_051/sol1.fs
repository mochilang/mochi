// Generated 2025-08-12 13:41 +0700

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
let rec parse_int (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            value <- (value * 10) + (int (string (s.[i])))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and digit_replacements (number: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable number = number
    try
        let num_str: string = _str (number)
        let mutable counts: int array = unbox<int array> [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
        let mutable i: int = 0
        while i < (String.length (num_str)) do
            let d: int = int (string (num_str.[i]))
            counts.[d] <- (_idx counts (int d)) + 1
            i <- i + 1
        let mutable result: int array array = Array.empty<int array>
        let digits: string = "0123456789"
        let mutable digit: int = 0
        while digit < 10 do
            if (_idx counts (int digit)) > 1 then
                let mutable family: int array = Array.empty<int>
                let mutable repl: int = 0
                while repl < 10 do
                    let mutable new_str: string = ""
                    let mutable j: int = 0
                    while j < (String.length (num_str)) do
                        let c: string = string (num_str.[j])
                        if c = (string (digits.[digit])) then
                            new_str <- new_str + (string (digits.[repl]))
                        else
                            new_str <- new_str + c
                        j <- j + 1
                    family <- Array.append family [|(parse_int (new_str))|]
                    repl <- repl + 1
                result <- Array.append result [|family|]
            digit <- digit + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and is_prime (num: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num < 2 then
            __ret <- false
            raise Return
        if (((num % 2 + 2) % 2)) = 0 then
            __ret <- num = 2
            raise Return
        let mutable i: int = 3
        while (i * i) <= num do
            if (((num % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution (family_length: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable family_length = family_length
    try
        let candidate: int = 121313
        if not (is_prime (candidate)) then
            __ret <- -1
            raise Return
        let reps: int array array = digit_replacements (candidate)
        let mutable r: int = 0
        while r < (Seq.length (reps)) do
            let mutable family: int array = _idx reps (int r)
            let mutable count: int = 0
            let mutable min_prime: int = 0
            let mutable first: bool = true
            let mutable i: int = 0
            while i < (Seq.length (family)) do
                let num: int = _idx family (int i)
                if is_prime (num) then
                    if first then
                        min_prime <- num
                        first <- false
                    else
                        if num < min_prime then
                            min_prime <- num
                    count <- count + 1
                i <- i + 1
            if count = family_length then
                __ret <- min_prime
                raise Return
            r <- r + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (8))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
