// Generated 2025-08-11 17:23 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let DIGITS: string = "0123456789"
let rec is_lower (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (LOWER)) do
            if (string (LOWER.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and is_upper (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (UPPER)) do
            if (string (UPPER.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (DIGITS)) do
            if (string (DIGITS.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and is_alpha (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        if is_lower (ch) then
            __ret <- true
            raise Return
        if is_upper (ch) then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and is_alnum (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        if is_alpha (ch) then
            __ret <- true
            raise Return
        if is_digit (ch) then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and to_lower (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (UPPER)) do
            if (string (UPPER.[i])) = ch then
                __ret <- string (LOWER.[i])
                raise Return
            i <- i + 1
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
and camel_to_snake_case (input_str: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_str = input_str
    try
        let mutable snake_str: string = ""
        let mutable i: int = 0
        let mutable prev_is_digit: bool = false
        let mutable prev_is_alpha: bool = false
        while i < (String.length (input_str)) do
            let ch: string = string (input_str.[i])
            if is_upper (ch) then
                snake_str <- (snake_str + "_") + (to_lower (ch))
            else
                if prev_is_digit && (is_lower (ch)) then
                    snake_str <- (snake_str + "_") + ch
                else
                    if prev_is_alpha && (is_digit (ch)) then
                        snake_str <- (snake_str + "_") + ch
                    else
                        if not (is_alnum (ch)) then
                            snake_str <- snake_str + "_"
                        else
                            snake_str <- snake_str + ch
            prev_is_digit <- is_digit (ch)
            prev_is_alpha <- is_alpha (ch)
            i <- i + 1
        if ((String.length (snake_str)) > 0) && ((string (snake_str.[0])) = "_") then
            snake_str <- _substring snake_str (1) (String.length (snake_str))
        __ret <- snake_str
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (camel_to_snake_case ("someRandomString"))
        printfn "%s" (camel_to_snake_case ("SomeRandomStr#ng"))
        printfn "%s" (camel_to_snake_case ("123SomeRandom123String123"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
