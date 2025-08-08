// Generated 2025-08-08 16:34 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type OptionMatrix = {
    mutable _value: float array array
    mutable _ok: bool
}
let rec identity (n: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    try
        let mutable mat: float array array = Array.empty<float array>
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
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- mat
        raise Return
        __ret
    with
        | Return -> __ret
and transpose (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let rows: int = Seq.length (mat)
        let cols: int = Seq.length (_idx mat (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable j: int = 0
        while j < cols do
            let mutable row: float array = Array.empty<float>
            let mutable i: int = 0
            while i < rows do
                row <- Array.append row [|(_idx (_idx mat (i)) (j))|]
                i <- i + 1
            res <- Array.append res [|row|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matmul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx b (0))
        let inner: int = Seq.length (_idx a (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < inner do
                    sum <- sum + ((_idx (_idx a (i)) (k)) * (_idx (_idx b (k)) (j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and mat_sub (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let rows: int = Seq.length (a)
        let cols: int = Seq.length (_idx a (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|((_idx (_idx a (i)) (j)) - (_idx (_idx b (i)) (j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and inverse (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let n: int = Seq.length (mat)
        let id: float array array = identity (n)
        let mutable aug: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.append (_idx mat (i)) (_idx id (i))
            aug <- Array.append aug [|row|]
            i <- i + 1
        let mutable col: int = 0
        while col < n do
            let mutable pivot_row: float array = _idx aug (col)
            let pivot: float = _idx pivot_row (col)
            if pivot = 0.0 then
                failwith ("matrix is singular")
            let mutable j: int = 0
            while j < (2 * n) do
                pivot_row.[j] <- (_idx pivot_row (j)) / pivot
                j <- j + 1
            aug.[col] <- pivot_row
            let mutable r: int = 0
            while r < n do
                if r <> col then
                    let mutable row_r: float array = _idx aug (r)
                    let factor: float = _idx row_r (col)
                    j <- 0
                    while j < (2 * n) do
                        row_r.[j] <- (_idx row_r (j)) - (factor * (_idx pivot_row (j)))
                        j <- j + 1
                    aug.[r] <- row_r
                r <- r + 1
            col <- col + 1
        let mutable inv: float array array = Array.empty<float array>
        let mutable r: int = 0
        while r < n do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = n
            while c < (2 * n) do
                row <- Array.append row [|(_idx (_idx aug (r)) (c))|]
                c <- c + 1
            inv <- Array.append inv [|row|]
            r <- r + 1
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
and schur_complement (mat_a: float array array) (mat_b: float array array) (mat_c: float array array) (pseudo_inv: OptionMatrix) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat_a = mat_a
    let mutable mat_b = mat_b
    let mutable mat_c = mat_c
    let mutable pseudo_inv = pseudo_inv
    try
        let a_rows: int = Seq.length (mat_a)
        let a_cols: int = Seq.length (_idx mat_a (0))
        if a_rows <> a_cols then
            failwith ("Matrix A must be square")
        if a_rows <> (Seq.length (mat_b)) then
            failwith ("Expected the same number of rows for A and B")
        if (Seq.length (_idx mat_b (0))) <> (Seq.length (_idx mat_c (0))) then
            failwith ("Expected the same number of columns for B and C")
        let mutable a_inv: float array array = Array.empty<float array>
        if pseudo_inv._ok then
            a_inv <- pseudo_inv._value
        else
            a_inv <- inverse (mat_a)
        let bt: float array array = transpose (mat_b)
        let a_inv_b: float array array = matmul (a_inv) (mat_b)
        let bt_a_inv_b: float array array = matmul (bt) (a_inv_b)
        __ret <- mat_sub (mat_c) (bt_a_inv_b)
        raise Return
        __ret
    with
        | Return -> __ret
and print_matrix (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            let mutable row: float array = _idx mat (i)
            while j < (Seq.length (row)) do
                line <- line + (_str (_idx row (j)))
                if (j + 1) < (Seq.length (row)) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: float array array = [|[|1.0; 2.0|]; [|2.0; 1.0|]|]
        let b: float array array = [|[|0.0; 3.0|]; [|3.0; 0.0|]|]
        let mutable c: float array array = [|[|2.0; 1.0|]; [|6.0; 3.0|]|]
        let none: OptionMatrix = { _value = Array.empty<float array>; _ok = false }
        let s: float array array = schur_complement (a) (b) (c) (none)
        print_matrix (s)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
