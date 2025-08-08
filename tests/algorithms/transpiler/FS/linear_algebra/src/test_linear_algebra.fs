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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec int_to_string (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable num: int = n
        let mutable neg: bool = false
        if num < 0 then
            neg <- true
            num <- -num
        let mutable res: string = ""
        while num > 0 do
            let digit: int = ((num % 10 + 10) % 10)
            let ch: string = _substring "0123456789" digit (digit + 1)
            res <- ch + res
            num <- _floordiv num 10
        if neg then
            res <- "-" + res
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec float_to_string (x: float) (dec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable x = x
    let mutable dec = dec
    try
        let mutable neg: bool = false
        let mutable num: float = x
        if num < 0.0 then
            neg <- true
            num <- -num
        let int_part: int = int num
        let mutable res: string = int_to_string (int_part)
        if dec > 0 then
            res <- res + "."
            let mutable frac: float = num - (float int_part)
            let mutable i: int = 0
            while i < dec do
                frac <- frac * 10.0
                let digit: int = int frac
                res <- res + (_substring "0123456789" digit (digit + 1))
                frac <- frac - (float digit)
                i <- i + 1
        if neg then
            res <- "-" + res
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_component (v: int array) (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable v = v
    let mutable i = i
    try
        __ret <- _idx v (i)
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_str_int (v: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable v = v
    try
        let mutable s: string = "("
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            s <- s + (int_to_string (_idx v (i)))
            if (i + 1) < (Seq.length (v)) then
                s <- s + ","
            i <- i + 1
        s <- s + ")"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_str_float (v: float array) (dec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable v = v
    let mutable dec = dec
    try
        let mutable s: string = "("
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            s <- s + (float_to_string (_idx v (i)) (dec))
            if (i + 1) < (Seq.length (v)) then
                s <- s + ","
            i <- i + 1
        s <- s + ")"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_add (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) + (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_sub (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|((_idx a (i)) - (_idx b (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_scalar_mul (v: int array) (s: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable v = v
    let mutable s = s
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            res <- Array.append res [|((float (_idx v (i))) * s)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec vector_dot (a: int array) (b: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- sum + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrt_newton (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable low: float = 0.0
        let mutable high: float = x
        if x < 1.0 then
            high <- 1.0
        let mutable mid: float = 0.0
        let mutable i: int = 0
        while i < 40 do
            mid <- (low + high) / 2.0
            if (mid * mid) > x then
                high <- mid
            else
                low <- mid
            i <- i + 1
        __ret <- mid
        raise Return
        __ret
    with
        | Return -> __ret
let rec euclidean_length (v: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            let ``val``: float = float (_idx v (i))
            sum <- sum + (``val`` * ``val``)
            i <- i + 1
        __ret <- sqrt_newton (sum)
        raise Return
        __ret
    with
        | Return -> __ret
let rec zero_vector (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable v: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            v <- Array.append v [|0|]
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
let rec unit_basis_vector (n: int) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable idx = idx
    try
        let mutable v: int array = zero_vector (n)
        v.[idx] <- 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
let rec axpy (a: int) (x: int array) (y: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable x = x
    let mutable y = y
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            res <- Array.append res [|((a * (_idx x (i))) + (_idx y (i)))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec copy_vector (x: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable x = x
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            res <- Array.append res [|(_idx x (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec change_component (v: int array) (idx: int) (``val``: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable v = v
    let mutable idx = idx
    let mutable ``val`` = ``val``
    try
        v.[idx] <- ``val``
        __ret
    with
        | Return -> __ret
let rec matrix_str (m: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable m = m
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            s <- s + "|"
            let mutable j: int = 0
            while j < (Seq.length (_idx m (0))) do
                s <- s + (int_to_string (_idx (_idx m (i)) (j)))
                if (j + 1) < (Seq.length (_idx m (0))) then
                    s <- s + ","
                j <- j + 1
            s <- s + "|\n"
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec submatrix (m: int array array) (row: int) (col: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    let mutable row = row
    let mutable col = col
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            if i <> row then
                let mutable r: int array = Array.empty<int>
                let mutable j: int = 0
                while j < (Seq.length (_idx m (0))) do
                    if j <> col then
                        r <- Array.append r [|(_idx (_idx m (i)) (j))|]
                    j <- j + 1
                res <- Array.append res [|r|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec determinant (m: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    try
        let n: int = Seq.length (m)
        if n = 1 then
            __ret <- _idx (_idx m (0)) (0)
            raise Return
        if n = 2 then
            __ret <- ((_idx (_idx m (0)) (0)) * (_idx (_idx m (1)) (1))) - ((_idx (_idx m (0)) (1)) * (_idx (_idx m (1)) (0)))
            raise Return
        let mutable det: int = 0
        let mutable c: int = 0
        while c < n do
            let sub: int array array = submatrix (m) (0) (c)
            let mutable sign: int = 1
            if (((c % 2 + 2) % 2)) = 1 then
                sign <- -1
            det <- det + ((sign * (_idx (_idx m (0)) (c))) * (determinant (sub)))
            c <- c + 1
        __ret <- det
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_minor (m: int array array) (row: int) (col: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    let mutable row = row
    let mutable col = col
    try
        __ret <- determinant (submatrix (m) (row) (col))
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_cofactor (m: int array array) (row: int) (col: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    let mutable row = row
    let mutable col = col
    try
        let mutable sign: int = 1
        if ((((row + col) % 2 + 2) % 2)) = 1 then
            sign <- -1
        __ret <- sign * (matrix_minor (m) (row) (col))
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_mul_vector (m: int array array) (v: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable m = m
    let mutable v = v
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable sum: int = 0
            let mutable j: int = 0
            while j < (Seq.length (_idx m (0))) do
                sum <- sum + ((_idx (_idx m (i)) (j)) * (_idx v (j)))
                j <- j + 1
            res <- Array.append res [|sum|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_mul_scalar (m: int array array) (s: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    let mutable s = s
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx m (0))) do
                row <- Array.append row [|((_idx (_idx m (i)) (j)) * s)|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_change_component (m: int array array) (i: int) (j: int) (``val``: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    let mutable i = i
    let mutable j = j
    let mutable ``val`` = ``val``
    try
        m.[i].[j] <- ``val``
        __ret
    with
        | Return -> __ret
let rec matrix_component (m: int array array) (i: int) (j: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    let mutable i = i
    let mutable j = j
    try
        __ret <- _idx (_idx m (i)) (j)
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_add (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx a (0))) do
                row <- Array.append row [|((_idx (_idx a (i)) (j)) + (_idx (_idx b (i)) (j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_sub (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < (Seq.length (_idx a (0))) do
                row <- Array.append row [|((_idx (_idx a (i)) (j)) - (_idx (_idx b (i)) (j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec square_zero_matrix (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable m: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            m <- Array.append m [|(zero_vector (n))|]
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec assert_int (name: string) (actual: int) (expected: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable name = name
    let mutable actual = actual
    let mutable expected = expected
    try
        if actual = expected then
            printfn "%s" (name + " ok")
        else
            printfn "%s" ((((name + " fail ") + (int_to_string (actual))) + " != ") + (int_to_string (expected)))
        __ret
    with
        | Return -> __ret
let rec assert_str (name: string) (actual: string) (expected: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable name = name
    let mutable actual = actual
    let mutable expected = expected
    try
        if actual = expected then
            printfn "%s" (name + " ok")
        else
            printfn "%s" (name + " fail")
            printfn "%s" (actual)
            printfn "%s" (expected)
        __ret
    with
        | Return -> __ret
let rec assert_float (name: string) (actual: float) (expected: float) (eps: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable name = name
    let mutable actual = actual
    let mutable expected = expected
    let mutable eps = eps
    try
        let mutable diff: float = actual - expected
        if diff < 0.0 then
            diff <- -diff
        if diff <= eps then
            printfn "%s" (name + " ok")
        else
            printfn "%s" (name + " fail")
        __ret
    with
        | Return -> __ret
let vx: int array = unbox<int array> [|1; 2; 3|]
assert_int ("component0") (vector_component (vx) (0)) (1)
assert_int ("component2") (vector_component (vx) (2)) (3)
let vs: int array = unbox<int array> [|0; 0; 0; 0; 0; 1|]
assert_str ("str_vector") (vector_str_int (vs)) ("(0,0,0,0,0,1)")
let vsize: int array = unbox<int array> [|1; 2; 3; 4|]
assert_int ("size") (Seq.length (vsize)) (4)
let va: int array = unbox<int array> [|1; 2; 3|]
let vb: int array = unbox<int array> [|1; 1; 1|]
let vsum: int array = vector_add (va) (vb)
assert_int ("add0") (vector_component (vsum) (0)) (2)
assert_int ("add1") (vector_component (vsum) (1)) (3)
assert_int ("add2") (vector_component (vsum) (2)) (4)
let vsub: int array = vector_sub (va) (vb)
assert_int ("sub0") (vector_component (vsub) (0)) (0)
assert_int ("sub1") (vector_component (vsub) (1)) (1)
assert_int ("sub2") (vector_component (vsub) (2)) (2)
let vmul: float array = vector_scalar_mul (va) (3.0)
assert_str ("scalar_mul") (vector_str_float (vmul) (1)) ("(3.0,6.0,9.0)")
assert_int ("dot_product") (vector_dot (unbox<int array> [|2; -1; 4|]) (unbox<int array> [|1; -2; -1|])) (0)
let zvec: int array = zero_vector (10)
let zstr: string = vector_str_int (zvec)
let mutable zcount: int = 0
let mutable zi: int = 0
while zi < (String.length (zstr)) do
    if (_substring zstr zi (zi + 1)) = "0" then
        zcount <- zcount + 1
    zi <- zi + 1
assert_int ("zero_vector") (zcount) (10)
assert_str ("unit_basis") (vector_str_int (unit_basis_vector (3) (1))) ("(0,1,0)")
assert_str ("axpy") (vector_str_int (axpy (2) (unbox<int array> [|1; 2; 3|]) (unbox<int array> [|1; 0; 1|]))) ("(3,4,7)")
let vcopy: int array = copy_vector (unbox<int array> [|1; 0; 0; 0; 0; 0|])
assert_str ("copy") (vector_str_int (vcopy)) ("(1,0,0,0,0,0)")
let mutable vchange: int array = unbox<int array> [|1; 0; 0|]
change_component (vchange) (0) (0)
change_component (vchange) (1) (1)
assert_str ("change_component") (vector_str_int (vchange)) ("(0,1,0)")
let mutable ma: int array array = [|[|1; 2; 3|]; [|2; 4; 5|]; [|6; 7; 8|]|]
assert_str ("matrix_str") (matrix_str (ma)) ("|1,2,3|\n|2,4,5|\n|6,7,8|\n")
assert_int ("determinant") (determinant (ma)) (-5)
let mutable mb: int array array = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]|]
let mv: int array = matrix_mul_vector (mb) (unbox<int array> [|1; 2; 3|])
assert_str ("matrix_vec_mul") (vector_str_int (mv)) ("(14,32,50)")
let msc: int array array = matrix_mul_scalar (mb) (2)
assert_str ("matrix_scalar_mul") (matrix_str (msc)) ("|2,4,6|\n|8,10,12|\n|14,16,18|\n")
let mutable mc: int array array = [|[|1; 2; 3|]; [|2; 4; 5|]; [|6; 7; 8|]|]
matrix_change_component (mc) (0) (2) (5)
assert_str ("change_component_matrix") (matrix_str (mc)) ("|1,2,5|\n|2,4,5|\n|6,7,8|\n")
assert_int ("matrix_component") (matrix_component (mc) (2) (1)) (7)
let mutable madd: int array array = matrix_add ([|[|1; 2; 3|]; [|2; 4; 5|]; [|6; 7; 8|]|]) ([|[|1; 2; 7|]; [|2; 4; 5|]; [|6; 7; 10|]|])
assert_str ("matrix_add") (matrix_str (madd)) ("|2,4,10|\n|4,8,10|\n|12,14,18|\n")
let mutable msub: int array array = matrix_sub ([|[|1; 2; 3|]; [|2; 4; 5|]; [|6; 7; 8|]|]) ([|[|1; 2; 7|]; [|2; 4; 5|]; [|6; 7; 10|]|])
assert_str ("matrix_sub") (matrix_str (msub)) ("|0,0,-4|\n|0,0,0|\n|0,0,-2|\n")
let mzero: int array array = square_zero_matrix (5)
assert_str ("square_zero_matrix") (matrix_str (mzero)) ("|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
