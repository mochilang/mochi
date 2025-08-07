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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec build_set (words: string array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, bool> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, bool>>
    let mutable words = words
    try
        let mutable m: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        for w in Seq.map string (words) do
            m.[w] <- true
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec word_break (s: string) (words: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable words = words
    try
        let n: int = String.length (s)
        let dict: System.Collections.Generic.IDictionary<string, bool> = build_set (words)
        let mutable dp: bool array = [||]
        let mutable i: int = 0
        while i <= n do
            dp <- Array.append dp [|false|]
            i <- i + 1
        dp.[0] <- true
        i <- 1
        while i <= n do
            let mutable j: int = 0
            while j < i do
                if _idx dp (j) then
                    let sub: string = _substring s (j) (i)
                    if dict.ContainsKey(sub) then
                        dp.[i] <- true
                        j <- i
                j <- j + 1
            i <- i + 1
        __ret <- _idx dp (n)
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
print_bool (word_break ("applepenapple") (unbox<string array> [|"apple"; "pen"|]))
print_bool (word_break ("catsandog") (unbox<string array> [|"cats"; "dog"; "sand"; "and"; "cat"|]))
print_bool (word_break ("cars") (unbox<string array> [|"car"; "ca"; "rs"|]))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
