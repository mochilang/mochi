// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec encrypt (input_string: string) (key: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_string = input_string
    let mutable key = key
    try
        if key <= 0 then
            failwith ("Height of grid can't be 0 or negative")
        if (key = 1) || ((String.length (input_string)) <= key) then
            __ret <- input_string
            raise Return
        let lowest: int = key - 1
        let mutable temp_grid: string array array = [||]
        let mutable i: int = 0
        while i < key do
            temp_grid <- Array.append temp_grid [|Array.empty<string>|]
            i <- i + 1
        let mutable position: int = 0
        while position < (String.length (input_string)) do
            let mutable num: int = ((position % (lowest * 2) + (lowest * 2)) % (lowest * 2))
            let alt: int = (lowest * 2) - num
            if num > alt then
                num <- alt
            let mutable row: string array = _idx temp_grid (num)
            row <- Array.append row [|_substring input_string position (position + 1)|]
            temp_grid.[num] <- row
            position <- position + 1
        let mutable output: string = ""
        i <- 0
        while i < key do
            let mutable row: string array = _idx temp_grid (i)
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                output <- output + (_idx row (j))
                j <- j + 1
            i <- i + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt (input_string: string) (key: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_string = input_string
    let mutable key = key
    try
        if key <= 0 then
            failwith ("Height of grid can't be 0 or negative")
        if key = 1 then
            __ret <- input_string
            raise Return
        let lowest: int = key - 1
        let mutable counts: int array = [||]
        let mutable i: int = 0
        while i < key do
            counts <- Array.append counts [|0|]
            i <- i + 1
        let mutable pos: int = 0
        while pos < (String.length (input_string)) do
            let mutable num: int = ((pos % (lowest * 2) + (lowest * 2)) % (lowest * 2))
            let alt: int = (lowest * 2) - num
            if num > alt then
                num <- alt
            counts.[num] <- (_idx counts (num)) + 1
            pos <- pos + 1
        let mutable grid: string array array = [||]
        let mutable counter: int = 0
        i <- 0
        while i < key do
            let length: int = _idx counts (i)
            let slice: string = _substring input_string counter (counter + length)
            let mutable row: string array = [||]
            let mutable j: int = 0
            while j < (String.length (slice)) do
                row <- Array.append row [|string (slice.[j])|]
                j <- j + 1
            grid <- Array.append grid [|row|]
            counter <- counter + length
            i <- i + 1
        let mutable indices: int array = [||]
        i <- 0
        while i < key do
            indices <- Array.append indices [|0|]
            i <- i + 1
        let mutable output: string = ""
        pos <- 0
        while pos < (String.length (input_string)) do
            let mutable num: int = ((pos % (lowest * 2) + (lowest * 2)) % (lowest * 2))
            let alt: int = (lowest * 2) - num
            if num > alt then
                num <- alt
            output <- output + (_idx (_idx grid (num)) (_idx indices (num)))
            indices.[num] <- (_idx indices (num)) + 1
            pos <- pos + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let rec bruteforce (input_string: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, string> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, string>>
    let mutable input_string = input_string
    try
        let mutable results: System.Collections.Generic.IDictionary<int, string> = _dictCreate []
        let mutable key_guess: int = 1
        while key_guess < (String.length (input_string)) do
            results.[key_guess] <- decrypt (input_string) (key_guess)
            key_guess <- key_guess + 1
        __ret <- results
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (encrypt ("Hello World") (4))
printfn "%s" (decrypt ("HWe olordll") (4))
let bf: System.Collections.Generic.IDictionary<int, string> = bruteforce ("HWe olordll")
printfn "%s" (bf.[4])
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
