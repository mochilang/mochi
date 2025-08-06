// Generated 2025-08-06 23:33 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let default_alphabet: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt (input_string: string) (key: int) (alphabet: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_string = input_string
    let mutable key = key
    let mutable alphabet = alphabet
    try
        let mutable result: string = ""
        let mutable i: int = 0
        let n: int = String.length (alphabet)
        while i < (String.length (input_string)) do
            let ch: string = _substring input_string i (i + 1)
            let idx: int = index_of (alphabet) (ch)
            if idx < 0 then
                result <- result + ch
            else
                let mutable new_key: int = (((idx + key) % n + n) % n)
                if new_key < 0 then
                    new_key <- new_key + n
                result <- result + (_substring alphabet new_key (new_key + 1))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt (input_string: string) (key: int) (alphabet: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable input_string = input_string
    let mutable key = key
    let mutable alphabet = alphabet
    try
        let mutable result: string = ""
        let mutable i: int = 0
        let n: int = String.length (alphabet)
        while i < (String.length (input_string)) do
            let ch: string = _substring input_string i (i + 1)
            let idx: int = index_of (alphabet) (ch)
            if idx < 0 then
                result <- result + ch
            else
                let mutable new_key: int = (((idx - key) % n + n) % n)
                if new_key < 0 then
                    new_key <- new_key + n
                result <- result + (_substring alphabet new_key (new_key + 1))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and brute_force (input_string: string) (alphabet: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable input_string = input_string
    let mutable alphabet = alphabet
    try
        let mutable results: string array = [||]
        let mutable key: int = 1
        let n: int = String.length (alphabet)
        while key <= n do
            let message: string = decrypt (input_string) (key) (alphabet)
            results <- Array.append results [|message|]
            key <- key + 1
        __ret <- results
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let alpha: string = default_alphabet
        let enc: string = encrypt ("The quick brown fox jumps over the lazy dog") (8) (alpha)
        printfn "%s" (enc)
        let dec: string = decrypt (enc) (8) (alpha)
        printfn "%s" (dec)
        let brute: string array = brute_force ("jFyuMy xIH'N vLONy zILwy Gy!") (alpha)
        printfn "%s" (_idx brute (19))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
