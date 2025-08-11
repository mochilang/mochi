// Generated 2025-08-11 15:32 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
open System.Collections.Generic

let ETAOIN: string = "ETAOINSHRDLCUMWFGYPBVKJXQZ"
let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec etaoin_index (letter: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable letter = letter
    try
        let mutable i: int = 0
        while i < (String.length (ETAOIN)) do
            if (_substring ETAOIN i (i + 1)) = letter then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- String.length (ETAOIN)
        raise Return
        __ret
    with
        | Return -> __ret
and get_letter_count (message: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int>>
    let mutable message = message
    try
        let mutable letter_count: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            let c: string = _substring LETTERS i (i + 1)
            letter_count.[c] <- 0
            i <- i + 1
        let msg: string = message.ToUpper()
        let mutable j: int = 0
        while j < (String.length (msg)) do
            let ch: string = _substring msg j (j + 1)
            if LETTERS.Contains(ch) then
                letter_count.[ch] <- (_dictGet letter_count ((string (ch)))) + 1
            j <- j + 1
        __ret <- letter_count
        raise Return
        __ret
    with
        | Return -> __ret
and get_frequency_order (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable message = message
    try
        let letter_to_freq: System.Collections.Generic.IDictionary<string, int> = get_letter_count (message)
        let mutable max_freq: int = 0
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            let letter: string = _substring LETTERS i (i + 1)
            let f: int = _dictGet letter_to_freq ((string (letter)))
            if f > max_freq then
                max_freq <- f
            i <- i + 1
        let mutable result: string = ""
        let mutable freq: int = max_freq
        while freq >= 0 do
            let mutable group: string array = Array.empty<string>
            let mutable j: int = 0
            while j < (String.length (LETTERS)) do
                let letter: string = _substring LETTERS j (j + 1)
                if (_dictGet letter_to_freq ((string (letter)))) = freq then
                    group <- Array.append group [|letter|]
                j <- j + 1
            let mutable g_len: int = Seq.length (group)
            let mutable a: int = 0
            while a < g_len do
                let mutable b: int = 0
                while b < ((g_len - a) - 1) do
                    let g1: string = _idx group (int b)
                    let g2: string = _idx group (int (b + 1))
                    let idx1: int = etaoin_index (g1)
                    let idx2: int = etaoin_index (g2)
                    if idx1 < idx2 then
                        let tmp: string = _idx group (int b)
                        group.[int b] <- _idx group (int (b + 1))
                        group.[int (b + 1)] <- tmp
                    b <- b + 1
                a <- a + 1
            let mutable g: int = 0
            while g < (Seq.length (group)) do
                result <- result + (_idx group (int g))
                g <- g + 1
            freq <- freq - 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and english_freq_match_score (message: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable message = message
    try
        let freq_order: string = get_frequency_order (message)
        let top: string = _substring freq_order 0 6
        let bottom: string = _substring freq_order ((String.length (freq_order)) - 6) (String.length (freq_order))
        let mutable score: int = 0
        let mutable i: int = 0
        while i < 6 do
            let c: string = _substring ETAOIN i (i + 1)
            if top.Contains(c) then
                score <- score + 1
            i <- i + 1
        let mutable j: int = (String.length (ETAOIN)) - 6
        while j < (String.length (ETAOIN)) do
            let c: string = _substring ETAOIN j (j + 1)
            if bottom.Contains(c) then
                score <- score + 1
            j <- j + 1
        __ret <- score
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (get_frequency_order ("Hello World"))
        printfn "%d" (english_freq_match_score ("Hello World"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
