// Generated 2025-08-07 15:46 +0700

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
let rec make_bool_list (n: int) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    try
        let mutable row: bool array = [||]
        let mutable i: int = 0
        while i < n do
            row <- Array.append row [|false|]
            i <- i + 1
        __ret <- row
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_bool_matrix (rows: int) (cols: int) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable matrix: bool array array = [||]
        let mutable i: int = 0
        while i < rows do
            matrix <- Array.append matrix [|make_bool_list (cols)|]
            i <- i + 1
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_match (s: string) (p: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable p = p
    try
        let n: int = String.length (s)
        let m: int = String.length (p)
        let mutable dp: bool array array = make_bool_matrix (n + 1) (m + 1)
        dp.[0].[0] <- true
        let mutable j: int = 1
        while j <= m do
            if (_substring p (j - 1) (j)) = "*" then
                dp.[0].[j] <- _idx (_idx dp (0)) (j - 1)
            j <- j + 1
        let mutable i: int = 1
        while i <= n do
            let mutable j2: int = 1
            while j2 <= m do
                let pc: string = _substring p (j2 - 1) (j2)
                let sc: string = _substring s (i - 1) (i)
                if (pc = sc) || (pc = "?") then
                    dp.[i].[j2] <- _idx (_idx dp (i - 1)) (j2 - 1)
                else
                    if pc = "*" then
                        if (_idx (_idx dp (i - 1)) (j2)) || (_idx (_idx dp (i)) (j2 - 1)) then
                            dp.[i].[j2] <- true
                j2 <- j2 + 1
            i <- i + 1
        __ret <- _idx (_idx dp (n)) (m)
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
print_bool (is_match ("abc") ("a*c"))
print_bool (is_match ("abc") ("a*d"))
print_bool (is_match ("baaabab") ("*****ba*****ab"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
