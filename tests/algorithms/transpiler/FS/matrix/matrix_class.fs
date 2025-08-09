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
let rec make_matrix (values: float array array) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable values = values
    try
        let r: int = Seq.length (values)
        if r = 0 then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let c: int = Seq.length (_idx values (int 0))
        let mutable i: int = 0
        while i < r do
            if (Seq.length (_idx values (int i))) <> c then
                __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
                raise Return
            i <- i + 1
        __ret <- { _data = values; _rows = r; _cols = c }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_columns (m: Matrix) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let mutable _cols: float array array = Array.empty<float array>
        let mutable j: int = 0
        while j < (m._cols) do
            let mutable col: float array = Array.empty<float>
            let mutable i: int = 0
            while i < (m._rows) do
                col <- Array.append col [|(_idx (_idx (m._data) (int i)) (int j))|]
                i <- i + 1
            _cols <- Array.append _cols [|col|]
            j <- j + 1
        __ret <- _cols
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_identity (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                let v: float = if i = j then 1.0 else 0.0
                row <- Array.append row [|v|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_minor (m: Matrix) (r: int) (c: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    let mutable r = r
    let mutable c = c
    try
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            if i <> r then
                let mutable row: float array = Array.empty<float>
                let mutable j: int = 0
                while j < (m._cols) do
                    if j <> c then
                        row <- Array.append row [|(_idx (_idx (m._data) (int i)) (int j))|]
                    j <- j + 1
                vals <- Array.append vals [|row|]
            i <- i + 1
        let sub: Matrix = { _data = vals; _rows = (m._rows) - 1; _cols = (m._cols) - 1 }
        __ret <- matrix_determinant (sub)
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_cofactor (m: Matrix) (r: int) (c: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    let mutable r = r
    let mutable c = c
    try
        let minor: float = matrix_minor (m) (r) (c)
        if ((((r + c) % 2 + 2) % 2)) = 0 then
            __ret <- minor
            raise Return
        __ret <- (-1.0) * minor
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_minors (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                row <- Array.append row [|(matrix_minor (m) (i) (j))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_cofactors (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                row <- Array.append row [|(matrix_cofactor (m) (i) (j))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_determinant (m: Matrix) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        if (m._rows) <> (m._cols) then
            __ret <- 0.0
            raise Return
        if (m._rows) = 0 then
            __ret <- 0.0
            raise Return
        if (m._rows) = 1 then
            __ret <- _idx (_idx (m._data) (int 0)) (int 0)
            raise Return
        if (m._rows) = 2 then
            __ret <- ((_idx (_idx (m._data) (int 0)) (int 0)) * (_idx (_idx (m._data) (int 1)) (int 1))) - ((_idx (_idx (m._data) (int 0)) (int 1)) * (_idx (_idx (m._data) (int 1)) (int 0)))
            raise Return
        let mutable sum: float = 0.0
        let mutable j: int = 0
        while j < (m._cols) do
            sum <- sum + ((_idx (_idx (m._data) (int 0)) (int j)) * (matrix_cofactor (m) (0) (j)))
            j <- j + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_is_invertible (m: Matrix) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    try
        __ret <- (matrix_determinant (m)) <> 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_adjugate (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let cof: Matrix = matrix_cofactors (m)
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                row <- Array.append row [|(_idx (_idx (cof._data) (int j)) (int i))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_inverse (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        let det: float = matrix_determinant (m)
        if det = 0.0 then
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let adj: Matrix = matrix_adjugate (m)
        __ret <- matrix_mul_scalar (adj) (1.0 / det)
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_add_row (m: Matrix) (row: float array) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable row = row
    try
        let mutable newData: float array array = m._data
        newData <- Array.append newData [|row|]
        __ret <- { _data = newData; _rows = (m._rows) + 1; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_add_column (m: Matrix) (col: float array) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable col = col
    try
        let mutable newData: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            newData <- Array.append newData [|(Array.append (_idx (m._data) (int i)) [|(_idx col (int i))|])|]
            i <- i + 1
        __ret <- { _data = newData; _rows = m._rows; _cols = (m._cols) + 1 }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul_scalar (m: Matrix) (s: float) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable s = s
    try
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._cols) do
                row <- Array.append row [|((_idx (_idx (m._data) (int i)) (int j)) * s)|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = m._rows; _cols = m._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_neg (m: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    try
        __ret <- matrix_mul_scalar (m) (-1.0)
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
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._cols) do
                row <- Array.append row [|((_idx (_idx (a._data) (int i)) (int j)) + (_idx (_idx (b._data) (int i)) (int j)))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = a._rows; _cols = a._cols }
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
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._cols) do
                row <- Array.append row [|((_idx (_idx (a._data) (int i)) (int j)) - (_idx (_idx (b._data) (int i)) (int j)))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = a._rows; _cols = a._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_dot (row: float array) (col: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable row = row
    let mutable col = col
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (row)) do
            sum <- sum + ((_idx row (int i)) * (_idx col (int i)))
            i <- i + 1
        __ret <- sum
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
        let bcols: float array array = matrix_columns (b)
        let mutable vals: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._rows) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (b._cols) do
                row <- Array.append row [|(matrix_dot (_idx (a._data) (int i)) (_idx bcols (int j)))|]
                j <- j + 1
            vals <- Array.append vals [|row|]
            i <- i + 1
        __ret <- { _data = vals; _rows = a._rows; _cols = b._cols }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_pow (m: Matrix) (p: int) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable p = p
    try
        if p = 0 then
            __ret <- matrix_identity (m)
            raise Return
        if p < 0 then
            if matrix_is_invertible (m) then
                __ret <- matrix_pow (matrix_inverse (m)) (-p)
                raise Return
            __ret <- { _data = Array.empty<float array>; _rows = 0; _cols = 0 }
            raise Return
        let mutable result: Matrix = m
        let mutable i: int = 1
        while i < p do
            result <- matrix_mul (result) (m)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_to_string (m: Matrix) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        if (m._rows) = 0 then
            __ret <- "[]"
            raise Return
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (m._rows) do
            s <- s + "["
            let mutable j: int = 0
            while j < (m._cols) do
                s <- s + (_str (_idx (_idx (m._data) (int i)) (int j)))
                if j < ((m._cols) - 1) then
                    s <- s + " "
                j <- j + 1
            s <- s + "]"
            if i < ((m._rows) - 1) then
                s <- s + "\n "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let m: Matrix = make_matrix ([|[|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|]; [|7.0; 8.0; 9.0|]|])
        printfn "%s" (matrix_to_string (m))
        printfn "%s" (_str (matrix_columns (m)))
        printfn "%s" (((_str (m._rows)) + ",") + (_str (m._cols)))
        printfn "%s" (_str (matrix_is_invertible (m)))
        printfn "%s" (matrix_to_string (matrix_identity (m)))
        printfn "%s" (_str (matrix_determinant (m)))
        printfn "%s" (matrix_to_string (matrix_minors (m)))
        printfn "%s" (matrix_to_string (matrix_cofactors (m)))
        printfn "%s" (matrix_to_string (matrix_adjugate (m)))
        let m2: Matrix = matrix_mul_scalar (m) (3.0)
        printfn "%s" (matrix_to_string (m2))
        printfn "%s" (matrix_to_string (matrix_add (m) (m2)))
        printfn "%s" (matrix_to_string (matrix_sub (m) (m2)))
        printfn "%s" (matrix_to_string (matrix_pow (m) (3)))
        let m3: Matrix = matrix_add_row (m) (unbox<float array> [|10.0; 11.0; 12.0|])
        printfn "%s" (matrix_to_string (m3))
        let m4: Matrix = matrix_add_column (m2) (unbox<float array> [|8.0; 16.0; 32.0|])
        printfn "%s" (matrix_to_string (matrix_mul (m3) (m4)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
