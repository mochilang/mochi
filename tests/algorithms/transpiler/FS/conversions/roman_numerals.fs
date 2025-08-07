// Generated 2025-08-07 10:31 +0700

exception Break
exception Continue

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
let roman_values: int array = [|1000; 900; 500; 400; 100; 90; 50; 40; 10; 9; 5; 4; 1|]
let roman_symbols: string array = [|"M"; "CM"; "D"; "CD"; "C"; "XC"; "L"; "XL"; "X"; "IX"; "V"; "IV"; "I"|]
let rec char_value (c: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable c = c
    try
        if c = "I" then
            __ret <- 1
            raise Return
        if c = "V" then
            __ret <- 5
            raise Return
        if c = "X" then
            __ret <- 10
            raise Return
        if c = "L" then
            __ret <- 50
            raise Return
        if c = "C" then
            __ret <- 100
            raise Return
        if c = "D" then
            __ret <- 500
            raise Return
        if c = "M" then
            __ret <- 1000
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec roman_to_int (roman: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable roman = roman
    try
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (String.length (roman)) do
            if ((i + 1) < (String.length (roman))) && ((char_value (string (roman.[i]))) < (char_value (string (roman.[i + 1])))) then
                total <- (total + (char_value (string (roman.[i + 1])))) - (char_value (string (roman.[i])))
                i <- i + 2
            else
                total <- total + (char_value (string (roman.[i])))
                i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec int_to_roman (number: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number = number
    try
        let mutable num: int = number
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (Seq.length (roman_values)) do
                try
                    let value: int = _idx roman_values (i)
                    let symbol: string = _idx roman_symbols (i)
                    let factor: int = num / value
                    num <- ((num % value + value) % value)
                    let mutable j: int = 0
                    while j < factor do
                        res <- res + symbol
                        j <- j + 1
                    if num = 0 then
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
