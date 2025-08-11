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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec naive_string_search (text: string) (pattern: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable text = text
    let mutable pattern = pattern
    try
        let pat_len: int = String.length (pattern)
        let mutable positions: int array = Array.empty<int>
        let mutable i: int = 0
        try
            while i <= ((String.length (text)) - pat_len) do
                try
                    let mutable match_found: bool = true
                    let mutable j: int = 0
                    try
                        while j < pat_len do
                            try
                                if (string (text.[i + j])) <> (string (pattern.[j])) then
                                    match_found <- false
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if match_found then
                        positions <- Array.append positions [|i|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- positions
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (naive_string_search ("ABAAABCDBBABCDDEBCABC") ("ABC")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
