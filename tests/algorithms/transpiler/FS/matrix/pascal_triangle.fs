// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
let rec populate_current_row (triangle: int array array) (current_row_idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable triangle = triangle
    let mutable current_row_idx = current_row_idx
    try
        let mutable row: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= current_row_idx do
            if (i = 0) || (i = current_row_idx) then
                row <- Array.append row [|1|]
            else
                let left: int = _idx (_idx triangle (int (current_row_idx - 1))) (int (i - 1))
                let right: int = _idx (_idx triangle (int (current_row_idx - 1))) (int i)
                row <- Array.append row [|(left + right)|]
            i <- i + 1
        __ret <- row
        raise Return
        __ret
    with
        | Return -> __ret
and generate_pascal_triangle (num_rows: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable num_rows = num_rows
    try
        if num_rows <= 0 then
            __ret <- Array.empty<int array>
            raise Return
        let mutable triangle: int array array = Array.empty<int array>
        let mutable row_idx: int = 0
        while row_idx < num_rows do
            let mutable row: int array = populate_current_row (triangle) (row_idx)
            triangle <- Array.append triangle [|row|]
            row_idx <- row_idx + 1
        __ret <- triangle
        raise Return
        __ret
    with
        | Return -> __ret
and row_to_string (row: int array) (total_rows: int) (row_idx: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    let mutable total_rows = total_rows
    let mutable row_idx = row_idx
    try
        let mutable line: string = ""
        let mutable spaces: int = (total_rows - row_idx) - 1
        let mutable s: int = 0
        while s < spaces do
            line <- line + " "
            s <- s + 1
        let mutable c: int = 0
        while c <= row_idx do
            line <- line + (_str (_idx row (int c)))
            if c <> row_idx then
                line <- line + " "
            c <- c + 1
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
and print_pascal_triangle (num_rows: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable num_rows = num_rows
    try
        let mutable triangle: int array array = generate_pascal_triangle (num_rows)
        let mutable r: int = 0
        while r < num_rows do
            let mutable line: string = row_to_string (_idx triangle (int r)) (num_rows) (r)
            printfn "%s" (line)
            r <- r + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        print_pascal_triangle (5)
        printfn "%s" (_str (generate_pascal_triangle (5)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
