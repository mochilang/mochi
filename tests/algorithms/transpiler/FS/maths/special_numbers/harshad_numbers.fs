// Generated 2025-08-08 18:58 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try

        __ret
    with
        | Return -> __ret
and char_to_value (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        let digits: string = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable i: int = 0
        while i < (String.length (digits)) do
            if (string (digits.[i])) = c then
                __ret <- i
                raise Return
            i <- i + 1
        panic ("invalid digit")
        __ret
    with
        | Return -> __ret
and int_to_base (number: int) (``base``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable ``base`` = ``base``
    try
        if (``base`` < 2) || (``base`` > 36) then
            panic ("'base' must be between 2 and 36 inclusive")
        if number < 0 then
            panic ("number must be a positive integer")
        let digits: string = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable n: int = number
        let mutable result: string = ""
        while n > 0 do
            let remainder: int = ((n % ``base`` + ``base``) % ``base``)
            result <- (string (digits.[remainder])) + result
            n <- _floordiv n ``base``
        if result = "" then
            result <- "0"
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and base_to_int (num_str: string) (``base``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num_str = num_str
    let mutable ``base`` = ``base``
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (num_str)) do
            let c: string = string (num_str.[i])
            value <- int (((int64 value) * (int64 ``base``)) + (int64 (char_to_value (c))))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits (num: int) (``base``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    let mutable ``base`` = ``base``
    try
        if (``base`` < 2) || (``base`` > 36) then
            panic ("'base' must be between 2 and 36 inclusive")
        let num_str: string = int_to_base (num) (``base``)
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (String.length (num_str)) do
            let c: string = string (num_str.[i])
            total <- total + (char_to_value (c))
            i <- i + 1
        __ret <- int_to_base (total) (``base``)
        raise Return
        __ret
    with
        | Return -> __ret
and harshad_numbers_in_base (limit: int) (``base``: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable limit = limit
    let mutable ``base`` = ``base``
    try
        if (``base`` < 2) || (``base`` > 36) then
            panic ("'base' must be between 2 and 36 inclusive")
        if limit < 0 then
            __ret <- Array.empty<string>
            raise Return
        let mutable numbers: string array = Array.empty<string>
        let mutable i: int = 1
        while i < limit do
            let s: string = sum_of_digits (i) (``base``)
            let divisor: int = base_to_int (s) (``base``)
            if (((i % divisor + divisor) % divisor)) = 0 then
                numbers <- Array.append numbers [|(int_to_base (i) (``base``))|]
            i <- i + 1
        __ret <- numbers
        raise Return
        __ret
    with
        | Return -> __ret
and is_harshad_number_in_base (num: int) (``base``: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    let mutable ``base`` = ``base``
    try
        if (``base`` < 2) || (``base`` > 36) then
            panic ("'base' must be between 2 and 36 inclusive")
        if num < 0 then
            __ret <- false
            raise Return
        let mutable n: string = int_to_base (num) (``base``)
        let d: string = sum_of_digits (num) (``base``)
        let n_val: int = base_to_int (n) (``base``)
        let d_val: int = base_to_int (d) (``base``)
        __ret <- (((n_val % d_val + d_val) % d_val)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (int_to_base (0) (21))
        printfn "%s" (int_to_base (23) (2))
        printfn "%s" (int_to_base (58) (5))
        printfn "%s" (int_to_base (167) (16))
        printfn "%s" (sum_of_digits (103) (12))
        printfn "%s" (sum_of_digits (1275) (4))
        printfn "%s" (sum_of_digits (6645) (2))
        printfn "%s" (_repr (harshad_numbers_in_base (15) (2)))
        printfn "%s" (_repr (harshad_numbers_in_base (12) (34)))
        printfn "%s" (_repr (harshad_numbers_in_base (12) (4)))
        printfn "%b" (is_harshad_number_in_base (18) (10))
        printfn "%b" (is_harshad_number_in_base (21) (10))
        printfn "%b" (is_harshad_number_in_base (-21) (5))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
