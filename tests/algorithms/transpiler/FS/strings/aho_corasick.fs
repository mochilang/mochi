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

let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec search_all (text: string) (keywords: string array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int array>>
    let mutable text = text
    let mutable keywords = keywords
    try
        let mutable result: System.Collections.Generic.IDictionary<string, int array> = _dictCreate []
        for word in Seq.map string (keywords) do
            let mutable positions: int array = Array.empty<int>
            let m: int = String.length (word)
            let mutable i: int = 0
            while i <= ((String.length (text)) - m) do
                if (_substring text i (i + m)) = word then
                    positions <- Array.append positions [|i|]
                i <- i + 1
            if (Seq.length (positions)) > 0 then
                result.[word] <- positions
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let text: string = "whatever, err ... , wherever"
let keywords: string array = unbox<string array> [|"what"; "hat"; "ver"; "er"|]
printfn "%A" (search_all (text) (keywords))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
