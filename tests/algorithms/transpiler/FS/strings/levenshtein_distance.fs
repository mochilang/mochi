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
let rec range_list (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable lst: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            lst <- Array.append lst [|i|]
            i <- i + 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and min3 (a: int) (b: int) (c: int) =
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
and levenshtein_distance (first_word: string) (second_word: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable first_word = first_word
    let mutable second_word = second_word
    try
        if (String.length (first_word)) < (String.length (second_word)) then
            __ret <- levenshtein_distance (second_word) (first_word)
            raise Return
        if (String.length (second_word)) = 0 then
            __ret <- String.length (first_word)
            raise Return
        let mutable previous_row: int array = range_list ((String.length (second_word)) + 1)
        let mutable i: int = 0
        while i < (String.length (first_word)) do
            let c1: string = string (first_word.[i])
            let mutable current_row: int array = Array.empty<int>
            current_row <- Array.append current_row [|(i + 1)|]
            let mutable j: int = 0
            while j < (String.length (second_word)) do
                let c2: string = string (second_word.[j])
                let insertions: int = (_idx previous_row (int (j + 1))) + 1
                let deletions: int = (_idx current_row (int j)) + 1
                let substitutions: int = (_idx previous_row (int j)) + (if c1 = c2 then 0 else 1)
                let min_val: int = min3 (insertions) (deletions) (substitutions)
                current_row <- Array.append current_row [|min_val|]
                j <- j + 1
            previous_row <- current_row
            i <- i + 1
        __ret <- _idx previous_row (int ((Seq.length (previous_row)) - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and levenshtein_distance_optimized (first_word: string) (second_word: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable first_word = first_word
    let mutable second_word = second_word
    try
        if (String.length (first_word)) < (String.length (second_word)) then
            __ret <- levenshtein_distance_optimized (second_word) (first_word)
            raise Return
        if (String.length (second_word)) = 0 then
            __ret <- String.length (first_word)
            raise Return
        let mutable previous_row: int array = range_list ((String.length (second_word)) + 1)
        let mutable i: int = 0
        while i < (String.length (first_word)) do
            let c1: string = string (first_word.[i])
            let mutable current_row: int array = Array.empty<int>
            current_row <- Array.append current_row [|(i + 1)|]
            let mutable k: int = 0
            while k < (String.length (second_word)) do
                current_row <- Array.append current_row [|0|]
                k <- k + 1
            let mutable j: int = 0
            while j < (String.length (second_word)) do
                let c2: string = string (second_word.[j])
                let insertions: int = (_idx previous_row (int (j + 1))) + 1
                let deletions: int = (_idx current_row (int j)) + 1
                let substitutions: int = (_idx previous_row (int j)) + (if c1 = c2 then 0 else 1)
                let min_val: int = min3 (insertions) (deletions) (substitutions)
                current_row.[int (j + 1)] <- min_val
                j <- j + 1
            previous_row <- current_row
            i <- i + 1
        __ret <- _idx previous_row (int ((Seq.length (previous_row)) - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: string = "kitten"
        let b: string = "sitting"
        printfn "%s" (_str (levenshtein_distance (a) (b)))
        printfn "%s" (_str (levenshtein_distance_optimized (a) (b)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
