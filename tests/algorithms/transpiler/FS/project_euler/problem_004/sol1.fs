// Generated 2025-08-23 14:49 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_palindrome (num: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        let s: string = _str (num)
        let mutable i: int64 = int64 0
        let mutable j: int64 = int64 ((String.length (s)) - 1)
        while i < j do
            if (_substring s (int i) (int (i + (int64 1)))) <> (_substring s (int j) (int (j + (int64 1)))) then
                __ret <- false
                raise Return
            i <- i + (int64 1)
            j <- j - (int64 1)
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable number: int64 = n - (int64 1)
        while number > (int64 9999) do
            if is_palindrome (number) then
                let mutable divisor: int64 = int64 999
                while divisor > (int64 99) do
                    if (((number % divisor + divisor) % divisor)) = (int64 0) then
                        let other: int64 = _floordiv64 (int64 number) (int64 divisor)
                        if (String.length (_str (other))) = 3 then
                            __ret <- number
                            raise Return
                    divisor <- divisor - (int64 1)
            number <- number - (int64 1)
        ignore (printfn "%s" ("That number is larger than our acceptable range."))
        __ret <- int64 0
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("solution() = " + (_str (solution (int64 998001)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
