// Generated 2025-08-07 10:31 +0700

exception Break
exception Continue

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec list_contains (xs: string array) (v: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable v = v
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = v then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_power_of_two (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 1 then
            __ret <- false
            raise Return
        let mutable x: int = n
        while x > 1 do
            if (((x % 2 + 2) % 2)) <> 0 then
                __ret <- false
                raise Return
            x <- x / 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec bin_string (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable res: string = ""
        let mutable x: int = n
        while x > 0 do
            let bit: int = ((x % 2 + 2) % 2)
            res <- (_str (bit)) + res
            x <- x / 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec decompress_data (data_bits: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable data_bits = data_bits
    try
        let mutable lexicon: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("0", "0"); ("1", "1")]
        let mutable keys: string array = [|"0"; "1"|]
        let mutable result: string = ""
        let mutable curr_string: string = ""
        let mutable index: int = 2
        let mutable i: int = 0
        try
            while i < (String.length (data_bits)) do
                try
                    curr_string <- curr_string + (_substring data_bits i (i + 1))
                    if not (list_contains (keys) (curr_string)) then
                        i <- i + 1
                        raise Continue
                    let last_match_id: string = lexicon.[(string (curr_string))]
                    result <- result + last_match_id
                    lexicon.[curr_string] <- last_match_id + "0"
                    if is_power_of_two (index) then
                        let mutable new_lex: System.Collections.Generic.IDictionary<string, string> = _dictCreate []
                        let mutable new_keys: string array = [||]
                        let mutable j: int = 0
                        while j < (Seq.length (keys)) do
                            let curr_key: string = _idx keys (j)
                            new_lex.["0" + curr_key] <- lexicon.[(string (curr_key))]
                            new_keys <- Array.append new_keys [|"0" + curr_key|]
                            j <- j + 1
                        lexicon <- new_lex
                        keys <- new_keys
                    let new_key: string = bin_string (index)
                    lexicon.[new_key] <- last_match_id + "1"
                    keys <- Array.append keys [|new_key|]
                    index <- index + 1
                    curr_string <- ""
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let sample: string = "1011001"
let decompressed: string = decompress_data (sample)
printfn "%s" (decompressed)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
