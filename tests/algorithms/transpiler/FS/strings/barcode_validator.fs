// Generated 2025-08-13 12:32 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec has_alpha (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if ((c >= "a") && (c <= "z")) || ((c >= "A") && (c <= "Z")) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and parse_decimal (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let mutable value: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            let c: string = string (s.[i])
            if (c < "0") || (c > "9") then
                ignore (failwith ("Non-digit character encountered"))
            value <- (value * 10) + (int c)
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and get_barcode (barcode: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable barcode = barcode
    try
        if has_alpha (barcode) then
            ignore (failwith (("Barcode '" + barcode) + "' has alphabetic characters."))
        if ((String.length (barcode)) > 0) && ((string (barcode.[0])) = "-") then
            ignore (failwith ("The entered barcode has a negative value. Try again."))
        __ret <- parse_decimal (barcode)
        raise Return
        __ret
    with
        | Return -> __ret
and get_check_digit (barcode: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable barcode = barcode
    try
        let mutable num: int = _floordiv (int barcode) (int 10)
        let mutable s: int = 0
        let mutable position: int = 0
        while num <> 0 do
            let mult: int = if (((position % 2 + 2) % 2)) = 0 then 3 else 1
            s <- s + (mult * (((num % 10 + 10) % 10)))
            num <- _floordiv (int num) (int 10)
            position <- position + 1
        __ret <- (((10 - (((s % 10 + 10) % 10))) % 10 + 10) % 10)
        raise Return
        __ret
    with
        | Return -> __ret
and is_valid (barcode: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable barcode = barcode
    try
        __ret <- ((String.length (_str (barcode))) = 13) && ((get_check_digit (barcode)) = (int64 (((barcode % 10 + 10) % 10))))
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (get_check_digit (8718452538119L))))
ignore (printfn "%s" (_str (get_check_digit (int64 87184523))))
ignore (printfn "%s" (_str (get_check_digit (87193425381086L))))
let mutable res: int array = Array.empty<int>
let mutable x: int = 0
while x < 100 do
    res <- Array.append res [|int (get_check_digit (int64 x))|]
    x <- x + 10
ignore (printfn "%s" (_repr (res)))
ignore (printfn "%s" (_str (is_valid (8718452538119L))))
ignore (printfn "%s" (_str (is_valid (int64 87184525))))
ignore (printfn "%s" (_str (is_valid (87193425381089L))))
ignore (printfn "%s" (_str (is_valid (int64 0))))
ignore (printfn "%s" (_str (get_barcode ("8718452538119"))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
