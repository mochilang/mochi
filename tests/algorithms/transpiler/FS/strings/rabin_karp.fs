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
let alphabet_size: int = 256
let modulus: int = 1000003
let rec index_of_char (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let digits: string = "0123456789"
        let mutable idx: int = index_of_char (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- index_of_char (lower) (ch)
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        idx <- index_of_char (digits) (ch)
        if idx >= 0 then
            __ret <- 48 + idx
            raise Return
        if ch = "ü" then
            __ret <- 252
            raise Return
        if ch = "Ü" then
            __ret <- 220
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec rabin_karp (pattern: string) (text: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable pattern = pattern
    let mutable text = text
    try
        let p_len: int = String.length (pattern)
        let t_len: int = String.length (text)
        if p_len > t_len then
            __ret <- false
            raise Return
        let mutable p_hash: int = 0
        let mutable t_hash: int = 0
        let mutable modulus_power: int = 1
        let mutable i: int = 0
        while i < p_len do
            p_hash <- int (((((int64 (ord (string (pattern.[i])))) + ((int64 p_hash) * (int64 alphabet_size))) % (int64 modulus) + (int64 modulus)) % (int64 modulus)))
            t_hash <- int (((((int64 (ord (string (text.[i])))) + ((int64 t_hash) * (int64 alphabet_size))) % (int64 modulus) + (int64 modulus)) % (int64 modulus)))
            if i <> (p_len - 1) then
                modulus_power <- int (((((int64 modulus_power) * (int64 alphabet_size)) % (int64 modulus) + (int64 modulus)) % (int64 modulus)))
            i <- i + 1
        let mutable j: int = 0
        try
            while j <= (t_len - p_len) do
                try
                    if (t_hash = p_hash) && ((_substring text j (j + p_len)) = pattern) then
                        __ret <- true
                        raise Return
                    if j = (t_len - p_len) then
                        j <- j + 1
                        raise Continue
                    t_hash <- int (((((((int64 t_hash) - ((int64 (ord (string (text.[j])))) * (int64 modulus_power))) * (int64 alphabet_size)) + (int64 (ord (string (text.[j + p_len]))))) % (int64 modulus) + (int64 modulus)) % (int64 modulus)))
                    if t_hash < 0 then
                        t_hash <- t_hash + modulus
                    j <- j + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_rabin_karp () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let pattern1: string = "abc1abc12"
        let text1: string = "alskfjaldsabc1abc1abc12k23adsfabcabc"
        let text2: string = "alskfjaldsk23adsfabcabc"
        if (not (rabin_karp (pattern1) (text1))) || (rabin_karp (pattern1) (text2)) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        let pattern2: string = "ABABX"
        let text3: string = "ABABZABABYABABX"
        if not (rabin_karp (pattern2) (text3)) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        let pattern3: string = "AAAB"
        let text4: string = "ABAAAAAB"
        if not (rabin_karp (pattern3) (text4)) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        let pattern4: string = "abcdabcy"
        let text5: string = "abcxabcdabxabcdabcdabcy"
        if not (rabin_karp (pattern4) (text5)) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        let pattern5: string = "Lü"
        let text6: string = "Lüsai"
        if not (rabin_karp (pattern5) (text6)) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        let pattern6: string = "Lue"
        if rabin_karp (pattern6) (text6) then
            printfn "%s" ("Failure")
            __ret <- ()
            raise Return
        printfn "%s" ("Success.")
        __ret
    with
        | Return -> __ret
test_rabin_karp()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
