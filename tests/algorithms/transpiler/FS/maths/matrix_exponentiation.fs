// Generated 2025-08-12 08:17 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec identity (n: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    try
        let mutable i: int = 0
        let mutable mat: int array array = Array.empty<int array>
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                if i = j then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            mat <- Array.append mat [|row|]
            i <- i + 1
        __ret <- mat
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_mul (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let n: int = Seq.length (a)
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                let mutable cell: int = 0
                let mutable k: int = 0
                while k < n do
                    cell <- cell + ((_idx (_idx a (int i)) (int k)) * (_idx (_idx b (int k)) (int j)))
                    k <- k + 1
                row <- Array.append row [|cell|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_pow (``base``: int array array) (exp: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int array array = identity (Seq.length (``base``))
        let mutable b: int array array = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- matrix_mul (result) (b)
            b <- matrix_mul (b) (b)
            e <- _floordiv e 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and fibonacci_with_matrix_exponentiation (n: int) (f1: int) (f2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable f1 = f1
    let mutable f2 = f2
    try
        if n = 1 then
            __ret <- f1
            raise Return
        if n = 2 then
            __ret <- f2
            raise Return
        let ``base``: int array array = [|[|1; 1|]; [|1; 0|]|]
        let m: int array array = matrix_pow (``base``) (n - 2)
        __ret <- (f2 * (_idx (_idx m (int 0)) (int 0))) + (f1 * (_idx (_idx m (int 0)) (int 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and simple_fibonacci (n: int) (f1: int) (f2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable f1 = f1
    let mutable f2 = f2
    try
        if n = 1 then
            __ret <- f1
            raise Return
        if n = 2 then
            __ret <- f2
            raise Return
        let mutable a: int = f1
        let mutable b: int = f2
        let mutable count: int = n - 2
        while count > 0 do
            let tmp: int = a + b
            a <- b
            b <- tmp
            count <- count - 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (fibonacci_with_matrix_exponentiation (1) (5) (6)))
printfn "%s" (_str (fibonacci_with_matrix_exponentiation (2) (10) (11)))
printfn "%s" (_str (fibonacci_with_matrix_exponentiation (13) (0) (1)))
printfn "%s" (_str (fibonacci_with_matrix_exponentiation (10) (5) (9)))
printfn "%s" (_str (fibonacci_with_matrix_exponentiation (9) (2) (3)))
printfn "%s" (_str (simple_fibonacci (1) (5) (6)))
printfn "%s" (_str (simple_fibonacci (2) (10) (11)))
printfn "%s" (_str (simple_fibonacci (13) (0) (1)))
printfn "%s" (_str (simple_fibonacci (10) (5) (9)))
printfn "%s" (_str (simple_fibonacci (9) (2) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
