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
let rec damerau_levenshtein_distance (first_string: string) (second_string: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable first_string = first_string
    let mutable second_string = second_string
    try
        let len1: int = String.length (first_string)
        let len2: int = String.length (second_string)
        let mutable dp_matrix: int array array = Array.empty<int array>
        for _ in 0 .. ((len1 + 1) - 1) do
            let mutable row: int array = Array.empty<int>
            for _2 in 0 .. ((len2 + 1) - 1) do
                row <- Array.append row [|0|]
            dp_matrix <- Array.append dp_matrix [|row|]
        for i in 0 .. ((len1 + 1) - 1) do
            let mutable row: int array = _idx dp_matrix (int i)
            row.[int 0] <- i
            dp_matrix.[int i] <- row
        let mutable first_row: int array = _idx dp_matrix (int 0)
        for j in 0 .. ((len2 + 1) - 1) do
            first_row.[int j] <- j
        dp_matrix.[int 0] <- first_row
        for i in 1 .. ((len1 + 1) - 1) do
            let mutable row: int array = _idx dp_matrix (int i)
            let first_char: string = _substring first_string (i - 1) i
            for j in 1 .. ((len2 + 1) - 1) do
                let second_char: string = _substring second_string (j - 1) j
                let cost: int = if first_char = second_char then 0 else 1
                let mutable value: int = (_idx (_idx dp_matrix (int (i - 1))) (int j)) + 1
                let insertion: int = (_idx row (int (j - 1))) + 1
                if insertion < value then
                    value <- insertion
                let substitution: int = (_idx (_idx dp_matrix (int (i - 1))) (int (j - 1))) + cost
                if substitution < value then
                    value <- substitution
                row.[int j] <- value
                if (((i > 1) && (j > 1)) && ((_substring first_string (i - 1) i) = (_substring second_string (j - 2) (j - 1)))) && ((_substring first_string (i - 2) (i - 1)) = (_substring second_string (j - 1) j)) then
                    let transposition: int = (_idx (_idx dp_matrix (int (i - 2))) (int (j - 2))) + cost
                    if transposition < (_idx row (int j)) then
                        row.[int j] <- transposition
            dp_matrix.[int i] <- row
        __ret <- _idx (_idx dp_matrix (int len1)) (int len2)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (damerau_levenshtein_distance ("cat") ("cut")))
printfn "%s" (_str (damerau_levenshtein_distance ("kitten") ("sitting")))
printfn "%s" (_str (damerau_levenshtein_distance ("hello") ("world")))
printfn "%s" (_str (damerau_levenshtein_distance ("book") ("back")))
printfn "%s" (_str (damerau_levenshtein_distance ("container") ("containment")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
