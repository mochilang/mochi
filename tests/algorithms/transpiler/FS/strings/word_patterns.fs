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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec find_index (xs: string array) (x: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = x then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and get_word_pattern (word: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable word = word
    try
        let w: string = word.ToUpper()
        let mutable letters: string array = Array.empty<string>
        let mutable numbers: string array = Array.empty<string>
        let mutable next_num: int = 0
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (w)) do
            let ch: string = string (w.[i])
            let idx: int = find_index (letters) (ch)
            let mutable num_str: string = ""
            if idx >= 0 then
                num_str <- _idx numbers (int idx)
            else
                num_str <- _str (next_num)
                letters <- Array.append letters [|ch|]
                numbers <- Array.append numbers [|num_str|]
                next_num <- next_num + 1
            if i > 0 then
                res <- res + "."
            res <- res + num_str
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (get_word_pattern (""))
        printfn "%s" (get_word_pattern (" "))
        printfn "%s" (get_word_pattern ("pattern"))
        printfn "%s" (get_word_pattern ("word pattern"))
        printfn "%s" (get_word_pattern ("get word pattern"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
