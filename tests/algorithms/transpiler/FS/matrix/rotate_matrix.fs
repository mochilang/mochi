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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_matrix (row_size: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable row_size = row_size
    try
        let mutable size: int = abs_int (row_size)
        if size = 0 then
            size <- 4
        let mutable mat: int array array = Array.empty<int array>
        let mutable y: int = 0
        while y < size do
            let mutable row: int array = Array.empty<int>
            let mutable x: int = 0
            while x < size do
                row <- Array.append row [|int ((int64 (1 + x)) + ((int64 y) * (int64 size)))|]
                x <- x + 1
            mat <- Array.append mat [|row|]
            y <- y + 1
        __ret <- mat
        raise Return
        __ret
    with
        | Return -> __ret
let rec transpose (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let n: int = Seq.length (mat)
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(_idx (_idx mat (int j)) (int i))|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_row (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = (Seq.length (mat)) - 1
        while i >= 0 do
            result <- Array.append result [|(_idx mat (int i))|]
            i <- i - 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_column (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = (Seq.length (_idx mat (int i))) - 1
            while j >= 0 do
                row <- Array.append row [|(_idx (_idx mat (int i)) (int j))|]
                j <- j - 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate_90 (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let t: int array array = transpose (mat)
        let rr: int array array = reverse_row (t)
        __ret <- rr
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate_180 (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let rc: int array array = reverse_column (mat)
        let rr: int array array = reverse_row (rc)
        __ret <- rr
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate_270 (mat: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    try
        let t: int array array = transpose (mat)
        let rc: int array array = reverse_column (t)
        __ret <- rc
        raise Return
        __ret
    with
        | Return -> __ret
let rec row_to_string (row: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable line: string = ""
        let mutable i: int = 0
        while i < (Seq.length (row)) do
            if i = 0 then
                line <- _str (_idx row (int i))
            else
                line <- (line + " ") + (_str (_idx row (int i)))
            i <- i + 1
        __ret <- line
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_matrix (mat: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            printfn "%s" (row_to_string (_idx mat (int i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
let mutable mat: int array array = make_matrix (4)
printfn "%s" ("\norigin:\n")
print_matrix (mat)
printfn "%s" ("\nrotate 90 counterclockwise:\n")
let r90: int array array = rotate_90 (mat)
print_matrix (r90)
mat <- make_matrix (4)
printfn "%s" ("\norigin:\n")
print_matrix (mat)
printfn "%s" ("\nrotate 180:\n")
let r180: int array array = rotate_180 (mat)
print_matrix (r180)
mat <- make_matrix (4)
printfn "%s" ("\norigin:\n")
print_matrix (mat)
printfn "%s" ("\nrotate 270 counterclockwise:\n")
let r270: int array array = rotate_270 (mat)
print_matrix (r270)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
