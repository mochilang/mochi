// Generated 2025-08-11 15:32 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_pangram (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let mutable letters: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (input_str)) do
            let c: string = (string (input_str.[i])).ToLower()
            let is_new: bool = not (Seq.contains c letters)
            if (((c <> " ") && ("a" <= c)) && (c <= "z")) && is_new then
                letters <- Array.append letters [|c|]
            i <- i + 1
        __ret <- (Seq.length (letters)) = 26
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_pangram_faster (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let alphabet: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable flag: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < 26 do
            flag <- Array.append flag [|false|]
            i <- i + 1
        let mutable j: int = 0
        try
            while j < (String.length (input_str)) do
                try
                    let c: string = (string (input_str.[j])).ToLower()
                    let mutable k: int = 0
                    try
                        while k < 26 do
                            try
                                if (string (alphabet.[k])) = c then
                                    flag.[int k] <- true
                                    raise Break
                                k <- k + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    j <- j + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable t: int = 0
        while t < 26 do
            if not (_idx flag (int t)) then
                __ret <- false
                raise Return
            t <- t + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_pangram_fastest (input_str: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable input_str = input_str
    try
        let s: string = input_str.ToLower()
        let alphabet: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable i: int = 0
        while i < (String.length (alphabet)) do
            let letter: string = string (alphabet.[i])
            if not (s.Contains(letter)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let s1: string = "The quick brown fox jumps over the lazy dog"
let s2: string = "My name is Unknown"
printfn "%s" (_str (is_pangram (s1)))
printfn "%s" (_str (is_pangram (s2)))
printfn "%s" (_str (is_pangram_faster (s1)))
printfn "%s" (_str (is_pangram_faster (s2)))
printfn "%s" (_str (is_pangram_fastest (s1)))
printfn "%s" (_str (is_pangram_fastest (s2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
