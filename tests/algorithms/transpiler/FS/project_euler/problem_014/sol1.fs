// Generated 2025-08-23 15:31 +0700

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
let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rec solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable counters: int64 array = Array.empty<int64>
        let mutable i: int64 = 0L
        while i <= n do
            counters <- Array.append counters [|0L|]
            i <- i + 1L
        counters.[int 1L] <- 1L
        let mutable largest_number: int64 = 1L
        let mutable pre_counter: int64 = 1L
        let mutable start: int64 = 2L
        try
            while start < n do
                try
                    let mutable number: int64 = start
                    let mutable counter: int64 = 0L
                    try
                        while true do
                            try
                                if (number < (int64 (Seq.length (counters)))) && ((_idx counters (int number)) <> 0L) then
                                    counter <- counter + (_idx counters (int number))
                                    raise Break
                                if (((number % 2L + 2L) % 2L)) = 0L then
                                    number <- _floordiv64 (int64 number) (int64 2L)
                                else
                                    number <- (3L * number) + 1L
                                counter <- counter + 1L
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if (start < (int64 (Seq.length (counters)))) && ((_idx counters (int start)) = 0L) then
                        counters.[int start] <- counter
                    if counter > pre_counter then
                        largest_number <- start
                        pre_counter <- counter
                    start <- start + 1L
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- largest_number
        raise Return
        __ret
    with
        | Return -> __ret
let input_str: string = _readLine()
let n: int64 = int (input_str)
ignore (printfn "%s" (_str (solution (n))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
