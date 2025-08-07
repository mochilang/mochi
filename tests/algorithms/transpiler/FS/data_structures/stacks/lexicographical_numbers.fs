// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec lexical_order (max_number: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable max_number = max_number
    try
        let mutable result: int array = Array.empty<int>
        let mutable stack: int array = unbox<int array> [|1|]
        try
            while (Seq.length (stack)) > 0 do
                try
                    let idx: int = (Seq.length (stack)) - 1
                    let num: int = _idx stack (idx)
                    stack <- Array.sub stack 0 (idx - 0)
                    if num > max_number then
                        raise Continue
                    result <- Array.append result [|num|]
                    if (((num % 10 + 10) % 10)) <> 9 then
                        stack <- Array.append stack [|num + 1|]
                    stack <- Array.append stack [|num * 10|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec join_ints (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if i > 0 then
                res <- res + " "
            res <- res + (_str (_idx xs (i)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (join_ints (lexical_order (13)))
printfn "%s" (_str (lexical_order (1)))
printfn "%s" (join_ints (lexical_order (20)))
printfn "%s" (join_ints (lexical_order (25)))
printfn "%s" (_str (lexical_order (12)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
