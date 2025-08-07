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
let rec helper (word1: string) (word2: string) (cache: int array array) (i: int) (j: int) (len1: int) (len2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word1 = word1
    let mutable word2 = word2
    let mutable cache = cache
    let mutable i = i
    let mutable j = j
    let mutable len1 = len1
    let mutable len2 = len2
    try
        if i >= len1 then
            __ret <- len2 - j
            raise Return
        if j >= len2 then
            __ret <- len1 - i
            raise Return
        if (_idx (_idx cache (i)) (j)) <> (0 - 1) then
            __ret <- _idx (_idx cache (i)) (j)
            raise Return
        let mutable diff: int = 0
        if (_substring word1 i (i + 1)) <> (_substring word2 j (j + 1)) then
            diff <- 1
        let delete_cost: int = 1 + (helper (word1) (word2) (cache) (i + 1) (j) (len1) (len2))
        let insert_cost: int = 1 + (helper (word1) (word2) (cache) (i) (j + 1) (len1) (len2))
        let replace_cost: int = diff + (helper (word1) (word2) (cache) (i + 1) (j + 1) (len1) (len2))
        cache.[i].[j] <- min3 (delete_cost) (insert_cost) (replace_cost)
        __ret <- _idx (_idx cache (i)) (j)
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_distance_up_bottom (word1: string) (word2: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable word1 = word1
    let mutable word2 = word2
    try
        let len1: int = String.length (word1)
        let len2: int = String.length (word2)
        let mutable cache: int array array = [||]
        for _ in 0 .. (len1 - 1) do
            let mutable row: int array = [||]
            for _2 in 0 .. (len2 - 1) do
                row <- Array.append row [|0 - 1|]
            cache <- Array.append cache [|row|]
        __ret <- helper (word1) (word2) (cache) (0) (0) (len1) (len2)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (min_distance_up_bottom ("intention") ("execution")))
printfn "%s" (_str (min_distance_up_bottom ("intention") ("")))
printfn "%s" (_str (min_distance_up_bottom ("") ("")))
printfn "%s" (_str (min_distance_up_bottom ("zooicoarchaeologist") ("zoologist")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
