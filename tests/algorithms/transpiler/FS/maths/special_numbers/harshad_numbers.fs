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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
     .Replace("L", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try

        __ret
    with
        | Return -> __ret
and char_to_value (c: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable c = c
    try
        let digits: string = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (digits))) do
            if (string (digits.[int i])) = c then
                __ret <- i
                raise Return
            i <- i + (int64 1)
        panic ("invalid digit")
        __ret
    with
        | Return -> __ret
and int_to_base (number: int64) (``base``: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable ``base`` = ``base``
    try
        if (``base`` < (int64 2)) || (``base`` > (int64 36)) then
            panic ("'base' must be between 2 and 36 inclusive")
        if number < (int64 0) then
            panic ("number must be a positive integer")
        let digits: string = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable n: int64 = number
        let mutable result: string = ""
        while n > (int64 0) do
            let remainder: int64 = ((n % ``base`` + ``base``) % ``base``)
            result <- (string (digits.[int remainder])) + result
            n <- _floordiv64 (int64 n) (int64 ``base``)
        if result = "" then
            result <- "0"
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and base_to_int (num_str: string) (``base``: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable num_str = num_str
    let mutable ``base`` = ``base``
    try
        let mutable value: int64 = int64 0
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (num_str))) do
            let c: string = string (num_str.[int i])
            value <- (value * ``base``) + (char_to_value (c))
            i <- i + (int64 1)
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits (num: int64) (``base``: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    let mutable ``base`` = ``base``
    try
        if (``base`` < (int64 2)) || (``base`` > (int64 36)) then
            panic ("'base' must be between 2 and 36 inclusive")
        let num_str: string = int_to_base (int64 num) (int64 ``base``)
        let mutable total: int64 = int64 0
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (num_str))) do
            let c: string = string (num_str.[int i])
            total <- total + (char_to_value (c))
            i <- i + (int64 1)
        __ret <- int_to_base (int64 total) (int64 ``base``)
        raise Return
        __ret
    with
        | Return -> __ret
and harshad_numbers_in_base (limit: int64) (``base``: int64) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable limit = limit
    let mutable ``base`` = ``base``
    try
        if (``base`` < (int64 2)) || (``base`` > (int64 36)) then
            panic ("'base' must be between 2 and 36 inclusive")
        if limit < (int64 0) then
            __ret <- Array.empty<string>
            raise Return
        let mutable numbers: string array = Array.empty<string>
        let mutable i: int64 = int64 1
        while i < limit do
            let s: string = sum_of_digits (int64 i) (int64 ``base``)
            let divisor: int64 = base_to_int (s) (int64 ``base``)
            if (((i % divisor + divisor) % divisor)) = (int64 0) then
                numbers <- Array.append numbers [|(int_to_base (int64 i) (int64 ``base``))|]
            i <- i + (int64 1)
        __ret <- numbers
        raise Return
        __ret
    with
        | Return -> __ret
and is_harshad_number_in_base (num: int64) (``base``: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    let mutable ``base`` = ``base``
    try
        if (``base`` < (int64 2)) || (``base`` > (int64 36)) then
            panic ("'base' must be between 2 and 36 inclusive")
        if num < (int64 0) then
            __ret <- false
            raise Return
        let mutable n: string = int_to_base (int64 num) (int64 ``base``)
        let d: string = sum_of_digits (int64 num) (int64 ``base``)
        let n_val: int64 = base_to_int (n) (int64 ``base``)
        let d_val: int64 = base_to_int (d) (int64 ``base``)
        __ret <- (((n_val % d_val + d_val) % d_val)) = (int64 0)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (int_to_base (int64 0) (int64 21)))
        ignore (printfn "%s" (int_to_base (int64 23) (int64 2)))
        ignore (printfn "%s" (int_to_base (int64 58) (int64 5)))
        ignore (printfn "%s" (int_to_base (int64 167) (int64 16)))
        ignore (printfn "%s" (sum_of_digits (int64 103) (int64 12)))
        ignore (printfn "%s" (sum_of_digits (int64 1275) (int64 4)))
        ignore (printfn "%s" (sum_of_digits (int64 6645) (int64 2)))
        ignore (printfn "%s" (_repr (harshad_numbers_in_base (int64 15) (int64 2))))
        ignore (printfn "%s" (_repr (harshad_numbers_in_base (int64 12) (int64 34))))
        ignore (printfn "%s" (_repr (harshad_numbers_in_base (int64 12) (int64 4))))
        ignore (printfn "%b" (is_harshad_number_in_base (int64 18) (int64 10)))
        ignore (printfn "%b" (is_harshad_number_in_base (int64 21) (int64 10)))
        ignore (printfn "%b" (is_harshad_number_in_base (int64 (-21)) (int64 5)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
