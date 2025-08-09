// Generated 2025-08-09 15:58 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = index_of (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- index_of (lower) (ch)
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 65) && (n < 91) then
            __ret <- _substring upper (n - 65) (n - 64)
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- _substring lower (n - 97) (n - 96)
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper_char (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let code: int = ord (c)
        if (code >= 97) && (code <= 122) then
            __ret <- chr (code - 32)
            raise Return
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_lower (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    try
        let code: int = ord (c)
        __ret <- (code >= 97) && (code <= 122)
        raise Return
        __ret
    with
        | Return -> __ret
let rec abbr (a: string) (b: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let n: int = String.length (a)
        let m: int = String.length (b)
        let mutable dp: bool array array = Array.empty<bool array>
        let mutable i: int = 0
        while i <= n do
            let mutable row: bool array = Array.empty<bool>
            let mutable j: int = 0
            while j <= m do
                row <- Array.append row [|false|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        dp.[int 0].[int 0] <- true
        i <- 0
        while i < n do
            let mutable j: int = 0
            while j <= m do
                if _idx (_idx dp (int i)) (int j) then
                    if (j < m) && ((to_upper_char (string (a.[i]))) = (string (b.[j]))) then
                        dp.[int (i + 1)].[int (j + 1)] <- true
                    if is_lower (string (a.[i])) then
                        dp.[int (i + 1)].[int j] <- true
                j <- j + 1
            i <- i + 1
        __ret <- _idx (_idx dp (int n)) (int m)
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_bool (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        if b then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        __ret
    with
        | Return -> __ret
print_bool (abbr ("daBcd") ("ABC"))
print_bool (abbr ("dBcd") ("ABC"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
