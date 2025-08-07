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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec octal_to_binary (octal_number: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable octal_number = octal_number
    try
        if (String.length (octal_number)) = 0 then
            failwith ("Empty string was passed to the function")
        let octal_digits: string = "01234567"
        let mutable binary_number: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (octal_number)) do
                try
                    let digit: string = string (octal_number.[i])
                    let mutable valid: bool = false
                    let mutable j: int = 0
                    try
                        while j < (String.length (octal_digits)) do
                            try
                                if digit = (string (octal_digits.[j])) then
                                    valid <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not valid then
                        failwith ("Non-octal value was passed to the function")
                    let mutable value: int = int digit
                    let mutable k: int = 0
                    let mutable binary_digit: string = ""
                    while k < 3 do
                        binary_digit <- (_str (((value % 2 + 2) % 2))) + binary_digit
                        value <- value / 2
                        k <- k + 1
                    binary_number <- binary_number + binary_digit
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- binary_number
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (octal_to_binary ("17"))
printfn "%s" (octal_to_binary ("7"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
