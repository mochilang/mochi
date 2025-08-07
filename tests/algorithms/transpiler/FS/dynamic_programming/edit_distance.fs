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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec min3 (a: int) (b: int) (c: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: int = a
        if b < m then
            m <- b
        if c < m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec helper_top_down (word1: string) (word2: string) (dp: int array array) (i: int) (j: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word1 = word1
    let mutable word2 = word2
    let mutable dp = dp
    let mutable i = i
    let mutable j = j
    try
        if i < 0 then
            __ret <- j + 1
            raise Return
        if j < 0 then
            __ret <- i + 1
            raise Return
        if (_idx (_idx dp (i)) (j)) <> (0 - 1) then
            __ret <- _idx (_idx dp (i)) (j)
            raise Return
        if (_substring word1 i (i + 1)) = (_substring word2 j (j + 1)) then
            dp.[i].[j] <- helper_top_down (word1) (word2) (dp) (i - 1) (j - 1)
        else
            let insert: int = helper_top_down (word1) (word2) (dp) (i) (j - 1)
            let delete: int = helper_top_down (word1) (word2) (dp) (i - 1) (j)
            let replace: int = helper_top_down (word1) (word2) (dp) (i - 1) (j - 1)
            dp.[i].[j] <- 1 + (min3 (insert) (delete) (replace))
        __ret <- _idx (_idx dp (i)) (j)
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_dist_top_down (word1: string) (word2: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word1 = word1
    let mutable word2 = word2
    try
        let mutable m: int = String.length (word1)
        let n: int = String.length (word2)
        let mutable dp: int array array = [||]
        for _ in 0 .. (m - 1) do
            let mutable row: int array = [||]
            for _2 in 0 .. (n - 1) do
                row <- Array.append row [|0 - 1|]
            dp <- Array.append dp [|row|]
        __ret <- helper_top_down (word1) (word2) (dp) (m - 1) (n - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_dist_bottom_up (word1: string) (word2: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word1 = word1
    let mutable word2 = word2
    try
        let mutable m: int = String.length (word1)
        let n: int = String.length (word2)
        let mutable dp: int array array = [||]
        for _ in 0 .. ((m + 1) - 1) do
            let mutable row: int array = [||]
            for _2 in 0 .. ((n + 1) - 1) do
                row <- Array.append row [|0|]
            dp <- Array.append dp [|row|]
        for i in 0 .. ((m + 1) - 1) do
            for j in 0 .. ((n + 1) - 1) do
                if i = 0 then
                    dp.[i].[j] <- j
                else
                    if j = 0 then
                        dp.[i].[j] <- i
                    else
                        if (_substring word1 (i - 1) i) = (_substring word2 (j - 1) j) then
                            dp.[i].[j] <- _idx (_idx dp (i - 1)) (j - 1)
                        else
                            let insert: int = _idx (_idx dp (i)) (j - 1)
                            let delete: int = _idx (_idx dp (i - 1)) (j)
                            let replace: int = _idx (_idx dp (i - 1)) (j - 1)
                            dp.[i].[j] <- 1 + (min3 (insert) (delete) (replace))
        __ret <- _idx (_idx dp (m)) (n)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (min_dist_top_down ("intention") ("execution")))
printfn "%s" (_str (min_dist_top_down ("intention") ("")))
printfn "%s" (_str (min_dist_top_down ("") ("")))
printfn "%s" (_str (min_dist_bottom_up ("intention") ("execution")))
printfn "%s" (_str (min_dist_bottom_up ("intention") ("")))
printfn "%s" (_str (min_dist_bottom_up ("") ("")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
