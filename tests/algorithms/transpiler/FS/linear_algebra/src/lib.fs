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
type Vector = {
    mutable _components: float array
}
type Matrix = {
    mutable _data: float array array
    mutable _width: int
    mutable _height: int
}
let PI: float = 3.141592653589793
let mutable _seed: int = 123456789
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let r: int = (((rand()) % ((b - a) + 1) + ((b - a) + 1)) % ((b - a) + 1))
        __ret <- a + r
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and arcsin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n < 10 do
            let num: float = (((((2.0 * (float n)) - 1.0) * ((2.0 * (float n)) - 1.0)) * x) * x) * term
            let den: float = (2.0 * (float n)) * ((2.0 * (float n)) + 1.0)
            term <- num / den
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and acos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (PI / 2.0) - (arcsin_taylor (x))
        raise Return
        __ret
    with
        | Return -> __ret
and vector_len (v: Vector) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable v = v
    try
        __ret <- Seq.length (v._components)
        raise Return
        __ret
    with
        | Return -> __ret
and vector_to_string (v: Vector) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable v = v
    try
        let mutable s: string = "("
        let mutable i: int = 0
        while i < (Seq.length (v._components)) do
            s <- s + (_str (_idx (v._components) (i)))
            if i < ((Seq.length (v._components)) - 1) then
                s <- s + ","
            i <- i + 1
        s <- s + ")"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and vector_add (a: Vector) (b: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable a = a
    let mutable b = b
    try
        let size: int = vector_len (a)
        if size <> (vector_len (b)) then
            __ret <- { _components = Array.empty<float> }
            raise Return
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < size do
            res <- Array.append res [|((_idx (a._components) (i)) + (_idx (b._components) (i)))|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and vector_sub (a: Vector) (b: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable a = a
    let mutable b = b
    try
        let size: int = vector_len (a)
        if size <> (vector_len (b)) then
            __ret <- { _components = Array.empty<float> }
            raise Return
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < size do
            res <- Array.append res [|((_idx (a._components) (i)) - (_idx (b._components) (i)))|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and vector_eq (a: Vector) (b: Vector) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (vector_len (a)) <> (vector_len (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (vector_len (a)) do
            if (_idx (a._components) (i)) <> (_idx (b._components) (i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and vector_mul_scalar (v: Vector) (s: float) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable v = v
    let mutable s = s
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (vector_len (v)) do
            res <- Array.append res [|((_idx (v._components) (i)) * s)|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and vector_dot (a: Vector) (b: Vector) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let size: int = vector_len (a)
        if size <> (vector_len (b)) then
            __ret <- 0.0
            raise Return
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < size do
            sum <- sum + ((_idx (a._components) (i)) * (_idx (b._components) (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and vector_copy (v: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable v = v
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (vector_len (v)) do
            res <- Array.append res [|(_idx (v._components) (i))|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and vector_component (v: Vector) (idx: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    let mutable idx = idx
    try
        __ret <- _idx (v._components) (idx)
        raise Return
        __ret
    with
        | Return -> __ret
and vector_change_component (v: Vector) (pos: int) (value: float) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable v = v
    let mutable pos = pos
    let mutable value = value
    try
        let mutable comps: float array = v._components
        comps.[pos] <- value
        __ret <- { _components = comps }
        raise Return
        __ret
    with
        | Return -> __ret
and vector_euclidean_length (v: Vector) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (v._components)) do
            sum <- sum + ((_idx (v._components) (i)) * (_idx (v._components) (i)))
            i <- i + 1
        let mutable result: float = sqrtApprox (sum)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and vector_angle (a: Vector) (b: Vector) (deg: bool) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable deg = deg
    try
        let num: float = vector_dot (a) (b)
        let den: float = (vector_euclidean_length (a)) * (vector_euclidean_length (b))
        let mutable ang: float = acos_taylor (num / den)
        if deg then
            ang <- (ang * 180.0) / PI
        __ret <- ang
        raise Return
        __ret
    with
        | Return -> __ret
and zero_vector (d: int) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable d = d
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < d do
            res <- Array.append res [|0.0|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and unit_basis_vector (d: int) (pos: int) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable d = d
    let mutable pos = pos
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < d do
            if i = pos then
                res <- Array.append res [|1.0|]
            else
                res <- Array.append res [|0.0|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and axpy (s: float) (x: Vector) (y: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable s = s
    let mutable x = x
    let mutable y = y
    try
        __ret <- vector_add (vector_mul_scalar (x) (s)) (y)
        raise Return
        __ret
    with
        | Return -> __ret
and random_vector (n: int) (a: int) (b: int) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable n = n
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|(float (random_int (a) (b)))|]
            i <- i + 1
        __ret <- { _components = res }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_to_string (m: Matrix) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable ans: string = ""
        let mutable i: int = 0
        while i < (m._height) do
            ans <- ans + "|"
            let mutable j: int = 0
            while j < (m._width) do
                ans <- ans + (_str (_idx (_idx (m._data) (i)) (j)))
                if j < ((m._width) - 1) then
                    ans <- ans + ","
                j <- j + 1
            ans <- ans + "|\n"
            i <- i + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_add (a: Matrix) (b: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable a = a
    let mutable b = b
    try
        if ((a._width) <> (b._width)) || ((a._height) <> (b._height)) then
            __ret <- { _data = Array.empty<float array>; _width = 0; _height = 0 }
            raise Return
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._height) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._width) do
                row <- Array.append row [|((_idx (_idx (a._data) (i)) (j)) + (_idx (_idx (b._data) (i)) (j)))|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- { _data = mat; _width = a._width; _height = a._height }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_sub (a: Matrix) (b: Matrix) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable a = a
    let mutable b = b
    try
        if ((a._width) <> (b._width)) || ((a._height) <> (b._height)) then
            __ret <- { _data = Array.empty<float array>; _width = 0; _height = 0 }
            raise Return
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (a._height) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (a._width) do
                row <- Array.append row [|((_idx (_idx (a._data) (i)) (j)) - (_idx (_idx (b._data) (i)) (j)))|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- { _data = mat; _width = a._width; _height = a._height }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul_vector (m: Matrix) (v: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable m = m
    let mutable v = v
    try
        if (Seq.length (v._components)) <> (m._width) then
            __ret <- { _components = Array.empty<float> }
            raise Return
        let mutable res: Vector = zero_vector (m._height)
        let mutable i: int = 0
        while i < (m._height) do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < (m._width) do
                sum <- sum + ((_idx (_idx (m._data) (i)) (j)) * (_idx (v._components) (j)))
                j <- j + 1
            res <- vector_change_component (res) (i) (sum)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul_scalar (m: Matrix) (s: float) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable s = s
    try
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._height) do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (m._width) do
                row <- Array.append row [|((_idx (_idx (m._data) (i)) (j)) * s)|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- { _data = mat; _width = m._width; _height = m._height }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_component (m: Matrix) (x: int) (y: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    let mutable x = x
    let mutable y = y
    try
        __ret <- _idx (_idx (m._data) (x)) (y)
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_change_component (m: Matrix) (x: int) (y: int) (value: float) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable m = m
    let mutable x = x
    let mutable y = y
    let mutable value = value
    try
        let mutable _data: float array array = m._data
        _data.[x].[y] <- value
        __ret <- { _data = _data; _width = m._width; _height = m._height }
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_minor (m: Matrix) (x: int) (y: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    let mutable x = x
    let mutable y = y
    try
        if (m._height) <> (m._width) then
            __ret <- 0.0
            raise Return
        let mutable minor: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (m._height) do
            if i <> x then
                let mutable row: float array = Array.empty<float>
                let mutable j: int = 0
                while j < (m._width) do
                    if j <> y then
                        row <- Array.append row [|(_idx (_idx (m._data) (i)) (j))|]
                    j <- j + 1
                minor <- Array.append minor [|row|]
            i <- i + 1
        let sub: Matrix = { _data = minor; _width = (m._width) - 1; _height = (m._height) - 1 }
        __ret <- matrix_determinant (sub)
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_cofactor (m: Matrix) (x: int) (y: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    let mutable x = x
    let mutable y = y
    try
        let sign: float = if ((((x + y) % 2 + 2) % 2)) = 0 then 1.0 else (-1.0)
        __ret <- sign * (matrix_minor (m) (x) (y))
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_determinant (m: Matrix) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable m = m
    try
        if (m._height) <> (m._width) then
            __ret <- 0.0
            raise Return
        if (m._height) = 0 then
            __ret <- 0.0
            raise Return
        if (m._height) = 1 then
            __ret <- _idx (_idx (m._data) (0)) (0)
            raise Return
        if (m._height) = 2 then
            __ret <- ((_idx (_idx (m._data) (0)) (0)) * (_idx (_idx (m._data) (1)) (1))) - ((_idx (_idx (m._data) (0)) (1)) * (_idx (_idx (m._data) (1)) (0)))
            raise Return
        let mutable sum: float = 0.0
        let mutable y: int = 0
        while y < (m._width) do
            sum <- sum + ((_idx (_idx (m._data) (0)) (y)) * (matrix_cofactor (m) (0) (y)))
            y <- y + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and square_zero_matrix (n: int) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable n = n
    try
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0.0|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- { _data = mat; _width = n; _height = n }
        raise Return
        __ret
    with
        | Return -> __ret
and random_matrix (w: int) (h: int) (a: int) (b: int) =
    let mutable __ret : Matrix = Unchecked.defaultof<Matrix>
    let mutable w = w
    let mutable h = h
    let mutable a = a
    let mutable b = b
    try
        let mutable mat: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < h do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < w do
                row <- Array.append row [|(float (random_int (a) (b)))|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- { _data = mat; _width = w; _height = h }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let v1: Vector = { _components = unbox<float array> [|1.0; 2.0; 3.0|] }
        let v2: Vector = { _components = unbox<float array> [|4.0; 5.0; 6.0|] }
        printfn "%s" (vector_to_string (vector_add (v1) (v2)))
        printfn "%s" (_str (vector_dot (v1) (v2)))
        printfn "%s" (_str (vector_euclidean_length (v1)))
        let m: Matrix = { _data = [|[|1.0; 2.0|]; [|3.0; 4.0|]|]; _width = 2; _height = 2 }
        printfn "%s" (_str (matrix_determinant (m)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
