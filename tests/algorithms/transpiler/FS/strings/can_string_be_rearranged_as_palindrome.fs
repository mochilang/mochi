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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let LETTERS: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LOWERCASE: string = "abcdefghijklmnopqrstuvwxyz"
let rec char_to_lower (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            if c = (_substring LETTERS i (i + 1)) then
                __ret <- _substring LOWERCASE i (i + 1)
                raise Return
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
and normalize (input_str: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_str = input_str
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (input_str)) do
            let ch: string = _substring input_str i (i + 1)
            let lc: string = char_to_lower (ch)
            if (lc >= "a") && (lc <= "z") then
                res <- res + lc
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and can_string_be_rearranged_as_palindrome_counter (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let s: string = normalize (input_str)
        let mutable freq: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if freq.ContainsKey(ch) then
                freq.[ch] <- (_dictGet freq ((string (ch)))) + 1
            else
                freq.[ch] <- 1
            i <- i + 1
        let mutable odd: int = 0
        for key in freq.Keys do
            if ((((_dictGet freq ((string (key)))) % 2 + 2) % 2)) <> 0 then
                odd <- odd + 1
        __ret <- odd < 2
        raise Return
        __ret
    with
        | Return -> __ret
and can_string_be_rearranged_as_palindrome (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let s: string = normalize (input_str)
        if (String.length (s)) = 0 then
            __ret <- true
            raise Return
        let mutable character_freq_dict: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable i: int = 0
        while i < (String.length (s)) do
            let character: string = _substring s i (i + 1)
            if character_freq_dict.ContainsKey(character) then
                character_freq_dict.[character] <- (_dictGet character_freq_dict ((string (character)))) + 1
            else
                character_freq_dict.[character] <- 1
            i <- i + 1
        let mutable odd_char: int = 0
        for character_key in character_freq_dict.Keys do
            let character_count: int = _dictGet character_freq_dict ((string (character_key)))
            if (((character_count % 2 + 2) % 2)) <> 0 then
                odd_char <- odd_char + 1
        __ret <- not (odd_char > 1)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (can_string_be_rearranged_as_palindrome_counter ("Momo"))
printfn "%b" (can_string_be_rearranged_as_palindrome_counter ("Mother"))
printfn "%b" (can_string_be_rearranged_as_palindrome ("Momo"))
printfn "%b" (can_string_be_rearranged_as_palindrome ("Mother"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
