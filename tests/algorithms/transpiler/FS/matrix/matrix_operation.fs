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
let rec add (matrices: float array array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrices = matrices
    try
        let rows: int = Seq.length (_idx matrices (int 0))
        let cols: int = Seq.length (_idx (_idx matrices (int 0)) (int 0))
        let mutable r: int = 0
        let mutable result: float array array = Array.empty<float array>
        while r < rows do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = 0
            while c < cols do
                let mutable sum: float = 0.0
                let mutable m: int = 0
                while m < (Seq.length (matrices)) do
                    sum <- sum + (_idx (_idx (_idx matrices (int m)) (int r)) (int c))
                    m <- m + 1
                row <- Array.append row [|sum|]
                c <- c + 1
            result <- Array.append result [|row|]
            r <- r + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and subtract (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (int 0))
        let mutable r: int = 0
        let mutable result: float array array = Array.empty<float array>
        while r < rows do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = 0
            while c < cols do
                row <- Array.append row [|((_idx (_idx a (int r)) (int c)) - (_idx (_idx b (int r)) (int c)))|]
                c <- c + 1
            result <- Array.append result [|row|]
            r <- r + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and scalar_multiply (matrix: float array array) (n: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    let mutable n = n
    try
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (matrix)) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_idx matrix (int i))) do
                row <- Array.append row [|((_idx (_idx matrix (int i)) (int j)) * n)|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and multiply (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rowsA: int = Seq.length (a)
        let colsA: int = Seq.length (_idx a (int 0))
        let rowsB: int = Seq.length (b)
        let colsB: int = Seq.length (_idx b (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rowsA do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < colsB do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < colsA do
                    sum <- sum + ((_idx (_idx a (int i)) (int k)) * (_idx (_idx b (int k)) (int j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and identity (n: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    try
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1.0|]
                else
                    row <- Array.append row [|0.0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and transpose (matrix: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    try
        let rows: int = Seq.length (matrix)
        let cols: int = Seq.length (_idx matrix (int 0))
        let mutable result: float array array = Array.empty<float array>
        let mutable c: int = 0
        while c < cols do
            let mutable row: float array = Array.empty<float>
            let mutable r: int = 0
            while r < rows do
                row <- Array.append row [|(_idx (_idx matrix (int r)) (int c))|]
                r <- r + 1
            result <- Array.append result [|row|]
            c <- c + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and minor (matrix: float array array) (row: int) (column: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    let mutable row = row
    let mutable column = column
    try
        let mutable result: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (matrix)) do
            if i <> row then
                let mutable new_row: float array = Array.empty<float>
                let mutable j: int = 0
                while j < (Seq.length (_idx matrix (int i))) do
                    if j <> column then
                        new_row <- Array.append new_row [|(_idx (_idx matrix (int i)) (int j))|]
                    j <- j + 1
                result <- Array.append result [|new_row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and determinant (matrix: float array array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable matrix = matrix
    try
        if (Seq.length (matrix)) = 1 then
            __ret <- _idx (_idx matrix (int 0)) (int 0)
            raise Return
        let mutable det: float = 0.0
        let mutable c: int = 0
        while c < (Seq.length (_idx matrix (int 0))) do
            let sub: float array array = minor (matrix) (0) (c)
            let sign: float = if (((c % 2 + 2) % 2)) = 0 then 1.0 else (-1.0)
            det <- det + (((_idx (_idx matrix (int 0)) (int c)) * (determinant (sub))) * sign)
            c <- c + 1
        __ret <- det
        raise Return
        __ret
    with
        | Return -> __ret
and inverse (matrix: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable matrix = matrix
    try
        let mutable det: float = determinant (matrix)
        if det = 0.0 then
            __ret <- Array.empty<float array>
            raise Return
        let size: int = Seq.length (matrix)
        let mutable matrix_minor: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < size do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < size do
                let mutable m: float array array = minor (matrix) (i) (j)
                row <- Array.append row [|(determinant (m))|]
                j <- j + 1
            matrix_minor <- Array.append matrix_minor [|row|]
            i <- i + 1
        let mutable cofactors: float array array = Array.empty<float array>
        i <- 0
        while i < size do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < size do
                let sign: float = if ((((i + j) % 2 + 2) % 2)) = 0 then 1.0 else (-1.0)
                row <- Array.append row [|((_idx (_idx matrix_minor (int i)) (int j)) * sign)|]
                j <- j + 1
            cofactors <- Array.append cofactors [|row|]
            i <- i + 1
        let adjugate: float array array = transpose (cofactors)
        __ret <- scalar_multiply (adjugate) (1.0 / det)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let matrix_a: float array array = [|[|12.0; 10.0|]; [|3.0; 9.0|]|]
        let matrix_b: float array array = [|[|3.0; 4.0|]; [|7.0; 4.0|]|]
        let matrix_c: float array array = [|[|11.0; 12.0; 13.0; 14.0|]; [|21.0; 22.0; 23.0; 24.0|]; [|31.0; 32.0; 33.0; 34.0|]; [|41.0; 42.0; 43.0; 44.0|]|]
        let matrix_d: float array array = [|[|3.0; 0.0; 2.0|]; [|2.0; 0.0; -2.0|]; [|0.0; 1.0; 1.0|]|]
        printfn "%s" (("Add Operation, add(matrix_a, matrix_b) = " + (_str (add ([|matrix_a; matrix_b|])))) + " \n")
        printfn "%s" (("Multiply Operation, multiply(matrix_a, matrix_b) = " + (_str (multiply (matrix_a) (matrix_b)))) + " \n")
        printfn "%s" (("Identity: " + (_str (identity (5)))) + "\n")
        printfn "%s" (((("Minor of " + (_str (matrix_c))) + " = ") + (_str (minor (matrix_c) (1) (2)))) + " \n")
        printfn "%s" (((("Determinant of " + (_str (matrix_b))) + " = ") + (_str (determinant (matrix_b)))) + " \n")
        printfn "%s" (((("Inverse of " + (_str (matrix_d))) + " = ") + (_str (inverse (matrix_d)))) + "\n")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
