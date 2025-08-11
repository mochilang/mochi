// Generated 2025-08-11 15:32 +0700

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

let rec validate_initial_digits (cc: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable cc = cc
    try
        __ret <- ((((((_substring cc (0) (2)) = "34") || ((_substring cc (0) (2)) = "35")) || ((_substring cc (0) (2)) = "37")) || ((_substring cc (0) (1)) = "4")) || ((_substring cc (0) (1)) = "5")) || ((_substring cc (0) (1)) = "6")
        raise Return
        __ret
    with
        | Return -> __ret
and luhn_validation (cc: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable cc = cc
    try
        let mutable sum: int = 0
        let mutable double_digit: bool = false
        let mutable i: int = (String.length (cc)) - 1
        while i >= 0 do
            let mutable n: int = int (_substring cc (i) (i + 1))
            if double_digit then
                n <- int ((int64 n) * (int64 2))
                if n > 9 then
                    n <- n - 9
            sum <- sum + n
            double_digit <- not double_digit
            i <- i - 1
        __ret <- (((sum % 10 + 10) % 10)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit_string (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = _substring s (i) (i + 1)
            if (c < "0") || (c > "9") then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and validate_credit_card_number (cc: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable cc = cc
    try
        let error_message: string = cc + " is an invalid credit card number because"
        if not (is_digit_string (cc)) then
            printfn "%s" (error_message + " it has nonnumerical characters.")
            __ret <- false
            raise Return
        if not (((String.length (cc)) >= 13) && ((String.length (cc)) <= 16)) then
            printfn "%s" (error_message + " of its length.")
            __ret <- false
            raise Return
        if not (validate_initial_digits (cc)) then
            printfn "%s" (error_message + " of its first two digits.")
            __ret <- false
            raise Return
        if not (luhn_validation (cc)) then
            printfn "%s" (error_message + " it fails the Luhn check.")
            __ret <- false
            raise Return
        printfn "%s" (cc + " is a valid credit card number.")
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        validate_credit_card_number ("4111111111111111")
        validate_credit_card_number ("32323")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
