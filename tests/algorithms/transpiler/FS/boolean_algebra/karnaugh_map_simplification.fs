// Generated 2025-08-06 21:33 +0700

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
let rec row_string (row: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (row)) do
            s <- s + (_str (_idx row (i)))
            if i < ((Seq.length (row)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and print_kmap (kmap: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable kmap = kmap
    try
        let mutable i: int = 0
        while i < (Seq.length (kmap)) do
            printfn "%s" (row_string (_idx kmap (i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and join_terms (terms: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable terms = terms
    try
        if (Seq.length (terms)) = 0 then
            __ret <- ""
            raise Return
        let mutable res: string = _idx terms (0)
        let mutable i: int = 1
        while i < (Seq.length (terms)) do
            res <- (res + " + ") + (_idx terms (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and simplify_kmap (board: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable board = board
    try
        let mutable terms: string array = [||]
        let mutable a: int = 0
        while a < (Seq.length (board)) do
            let row: int array = _idx board (a)
            let mutable b: int = 0
            while b < (Seq.length (row)) do
                let item: int = _idx row (b)
                if item <> 0 then
                    let term: string = (if a <> 0 then "A" else "A'") + (if b <> 0 then "B" else "B'")
                    terms <- Array.append terms [|term|]
                b <- b + 1
            a <- a + 1
        let expr: string = join_terms (terms)
        __ret <- expr
        raise Return
        __ret
    with
        | Return -> __ret
let mutable kmap: int array array = [|[|0; 1|]; [|1; 1|]|]
print_kmap (kmap)
printfn "%s" ("Simplified Expression:")
printfn "%s" (simplify_kmap (kmap))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
