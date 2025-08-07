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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec reverse (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable result: string = ""
        let mutable i: int = (String.length (s)) - 1
        while i >= 0 do
            result <- result + (_substring s (i) (i + 1))
            i <- i - 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec longest_palindromic_subsequence (s: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    try
        let rev: string = reverse (s)
        let n: int = String.length (s)
        let m: int = String.length (rev)
        let mutable dp: int array array = [||]
        let mutable i: int = 0
        while i <= n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j <= m do
                row <- Array.append row [|0|]
                j <- j + 1
            dp <- Array.append dp [|row|]
            i <- i + 1
        i <- 1
        while i <= n do
            let mutable j: int = 1
            while j <= m do
                let a_char: string = _substring s (i - 1) (i)
                let b_char: string = _substring rev (j - 1) (j)
                if a_char = b_char then
                    dp.[i].[j] <- 1 + (_idx (_idx dp (i - 1)) (j - 1))
                else
                    dp.[i].[j] <- max_int (_idx (_idx dp (i - 1)) (j)) (_idx (_idx dp (i)) (j - 1))
                j <- j + 1
            i <- i + 1
        __ret <- _idx (_idx dp (n)) (m)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (longest_palindromic_subsequence ("bbbab")))
printfn "%s" (_str (longest_palindromic_subsequence ("bbabcbcab")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
