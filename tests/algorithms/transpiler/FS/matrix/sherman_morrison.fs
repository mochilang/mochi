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
type Matrix = {
    mutable _data: float array array
    mutable _rows: int
    mutable _cols: int
}
let rec make_matrix (_rows: int) (_cols: int) (value: float) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable _rows = _rows
    let mutable _cols = _cols
    let mutable value = value
    try
        let mutable arr: float array array = Array.empty<float array>
        let mutable r: int = 0
        while r < _rows do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = 0
            while c < _cols do
                row <- Array.append row [|value|]
                c <- c + 1
            arr <- Array.append arr [|row|]
            r <- r + 1
        __ret <- { _data = arr; _rows = _rows; _cols = _cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_from_lists (vals: float array array) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable vals = vals
    try
        let mutable r: int = Seq.length (vals)
        let mutable c: int = if r = 0 then 0 else (Seq.length (_idx vals (int 0)))
        __ret <- { _data = vals; _rows = r; _cols = c }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_to_string (m: Matrix) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (m._rows) do
            s <- s + "["
            let mutable j: int = 0
            while j < (m._cols) do
                s <- s + (_str (_idx (_idx (m._data) (int i)) (int j)))
                if j < ((m._cols) - 1) then
                    s <- s + ", "
                j <- j + 1
            s <- s + "]"
            if i < ((m._rows) - 1) then
                s <- s + "\n"
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_add (a: Matrix) (b: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable a = a
    let mutable b = b
    try
        if ((a._rows) <> (b._rows)) || ((a._cols) <> (b._cols)) then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._cols) do
                row <- Array.append row [|((_idx (_idx (a._data) (int i)) (int j)) + (_idx (_idx (b._data) (int i)) (int j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- { _data = res; _rows = a._rows; _cols = a._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_sub (a: Matrix) (b: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable a = a
    let mutable b = b
    try
        if ((a._rows) <> (b._rows)) || ((a._cols) <> (b._cols)) then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._cols) do
                row <- Array.append row [|((_idx (_idx (a._data) (int i)) (int j)) - (_idx (_idx (b._data) (int i)) (int j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- { _data = res; _rows = a._rows; _cols = a._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul_scalar (m: Matrix) (k: float) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable k = k
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                row <- Array.append row [|((_idx (_idx (m._data) (int i)) (int j)) * k)|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- { _data = res; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul (a: Matrix) (b: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable a = a
    let mutable b = b
    try
        if (a._cols) <> (b._rows) then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (b._cols) do
                let mutable sum: float = 0.0
                let mutable k: int = 0
                while k < (a._cols) do
                    sum <- sum + ((_idx (_idx (a._data) (int i)) (int k)) * (_idx (_idx (b._data) (int k)) (int j)))
                    k <- k + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- { _data = res; _rows = a._rows; _cols = b._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_transpose (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable c: int = 0
        while c < (m._cols) do
            let mutable row: float array = Array.empty<float>
            let mutable r: int = 0
            while r < (m._rows) do
                row <- Array.append row [|(_idx (_idx (m._data) (int r)) (int c))|]
                r <- r + 1
            res <- Array.append res [|row|]
            c <- c + 1
        __ret <- { _data = res; _rows = m._cols; _cols = m._rows }
        raise Return
        __ret
    with
        | Return -> __ret
and sherman_morrison (ainv: Matrix) (u: Matrix) (v: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable ainv = ainv
    let mutable u = u
    let mutable v = v
    try
        let vt: Matrix = matrix_transpose (v)
        let vu: Matrix = matrix_mul (matrix_mul (vt) (ainv)) (u)
        let factor: float = (_idx (_idx (vu._data) (int 0)) (int 0)) + 1.0
        if factor = 0.0 then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let term1: Matrix = matrix_mul (ainv) (u)
        let term2: Matrix = matrix_mul (vt) (ainv)
        let numerator: Matrix = matrix_mul (term1) (term2)
        let scaled: Matrix = matrix_mul_scalar (numerator) (1.0 / factor)
        __ret <- matrix_sub (ainv) (scaled)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ainv: Matrix = matrix_from_lists ([|[|1.0; 0.0; 0.0|]; [|0.0; 1.0; 0.0|]; [|0.0; 0.0; 1.0|]|])
        let u: Matrix = matrix_from_lists ([|[|1.0|]; [|2.0|]; [|-3.0|]|])
        let v: Matrix = matrix_from_lists ([|[|4.0|]; [|-2.0|]; [|5.0|]|])
        let result: Matrix = sherman_morrison (ainv) (u) (v)
        printfn "%s" (matrix_to_string (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
