// Generated 2025-08-06 21:04 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec repeat_char (ch: string) (count: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable count = count
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < count do
            res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < exp do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and to_binary_no_prefix (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable v: int = n
        if v < 0 then
            v <- -v
        if v = 0 then
            __ret <- "0"
            raise Return
        let mutable res: string = ""
        while v > 0 do
            res <- (_str (((v % 2 + 2) % 2))) + res
            v <- v / 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and logical_left_shift (number: int) (shift_amount: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable shift_amount = shift_amount
    try
        if (number < 0) || (shift_amount < 0) then
            failwith ("both inputs must be positive integers")
        let mutable binary_number: string = "0b" + (to_binary_no_prefix (number))
        __ret <- binary_number + (repeat_char ("0") (shift_amount))
        raise Return
        __ret
    with
        | Return -> __ret
and logical_right_shift (number: int) (shift_amount: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable shift_amount = shift_amount
    try
        if (number < 0) || (shift_amount < 0) then
            failwith ("both inputs must be positive integers")
        let mutable binary_number: string = to_binary_no_prefix (number)
        if shift_amount >= (String.length (binary_number)) then
            __ret <- "0b0"
            raise Return
        let shifted: string = _substring binary_number 0 ((String.length (binary_number)) - shift_amount)
        __ret <- "0b" + shifted
        raise Return
        __ret
    with
        | Return -> __ret
and arithmetic_right_shift (number: int) (shift_amount: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    let mutable shift_amount = shift_amount
    try
        let mutable binary_number: string = ""
        if number >= 0 then
            binary_number <- "0" + (to_binary_no_prefix (number))
        else
            let length: int = String.length (to_binary_no_prefix (-number))
            let intermediate: int = (abs_int (number)) - (pow2 (length))
            let bin_repr: string = to_binary_no_prefix (intermediate)
            binary_number <- ("1" + (repeat_char ("0") (length - (String.length (bin_repr))))) + bin_repr
        if shift_amount >= (String.length (binary_number)) then
            let sign: string = _substring binary_number 0 1
            __ret <- "0b" + (repeat_char (sign) (String.length (binary_number)))
            raise Return
        let sign: string = _substring binary_number 0 1
        let shifted: string = _substring binary_number 0 ((String.length (binary_number)) - shift_amount)
        __ret <- ("0b" + (repeat_char (sign) (shift_amount))) + shifted
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (logical_left_shift (17) (2))
        printfn "%s" (logical_right_shift (1983) (4))
        printfn "%s" (arithmetic_right_shift (-17) (2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
