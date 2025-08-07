// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let ones: string array = [|"zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]
let teens: string array = [|"ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let tens: string array = [|""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
let short_powers: int array = [|15; 12; 9; 6; 3; 2|]
let short_units: string array = [|"quadrillion"; "trillion"; "billion"; "million"; "thousand"; "hundred"|]
let long_powers: int array = [|15; 9; 6; 3; 2|]
let long_units: string array = [|"billiard"; "milliard"; "million"; "thousand"; "hundred"|]
let indian_powers: int array = [|14; 12; 7; 5; 3; 2|]
let indian_units: string array = [|"crore crore"; "lakh crore"; "crore"; "lakh"; "thousand"; "hundred"|]
let rec pow10 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < exp do
            res <- res * 10
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_value (system: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable system = system
    try
        if system = "short" then
            __ret <- (pow10 (18)) - 1
            raise Return
        if system = "long" then
            __ret <- (pow10 (21)) - 1
            raise Return
        if system = "indian" then
            __ret <- (pow10 (19)) - 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec join_words (words: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable words = words
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (words)) do
            if i > 0 then
                res <- res + " "
            res <- res + (_idx words (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_small_number (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        if num < 0 then
            __ret <- ""
            raise Return
        if num >= 100 then
            __ret <- ""
            raise Return
        let tens_digit: int = num / 10
        let ones_digit: int = ((num % 10 + 10) % 10)
        if tens_digit = 0 then
            __ret <- _idx ones (ones_digit)
            raise Return
        if tens_digit = 1 then
            __ret <- _idx teens (ones_digit)
            raise Return
        let hyphen: string = if ones_digit > 0 then "-" else ""
        let tail: string = if ones_digit > 0 then (_idx ones (ones_digit)) else ""
        __ret <- ((_idx tens (tens_digit)) + hyphen) + tail
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_number (num: int) (system: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    let mutable system = system
    try
        let mutable word_groups: string array = [||]
        let mutable n: int = num
        if n < 0 then
            word_groups <- Array.append word_groups [|"negative"|]
            n <- -n
        if n > (max_value (system)) then
            __ret <- ""
            raise Return
        let mutable powers: int array = [||]
        let mutable units: string array = [||]
        if system = "short" then
            powers <- short_powers
            units <- short_units
        else
            if system = "long" then
                powers <- long_powers
                units <- long_units
            else
                if system = "indian" then
                    powers <- indian_powers
                    units <- indian_units
                else
                    __ret <- ""
                    raise Return
        let mutable i: int = 0
        while i < (Seq.length (powers)) do
            let power: int = _idx powers (i)
            let unit: string = _idx units (i)
            let divisor: int = pow10 (power)
            let digit_group: int = n / divisor
            n <- ((n % divisor + divisor) % divisor)
            if digit_group > 0 then
                let word_group: string = if digit_group >= 100 then (convert_number (digit_group) (system)) else (convert_small_number (digit_group))
                word_groups <- Array.append word_groups [|(word_group + " ") + unit|]
            i <- i + 1
        if (n > 0) || ((Seq.length (word_groups)) = 0) then
            word_groups <- Array.append word_groups [|convert_small_number (n)|]
        let joined: string = join_words (word_groups)
        __ret <- joined
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (convert_number (int 123456789012345L) ("short"))
printfn "%s" (convert_number (int 123456789012345L) ("long"))
printfn "%s" (convert_number (int 123456789012345L) ("indian"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
